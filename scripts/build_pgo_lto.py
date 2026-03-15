#!/usr/bin/env python3
"""Build a PGO+LTO-optimised pycomparse binary.

Always builds in a fresh temporary directory, so profile data and binary are
never out of sync.  The directory is left in place after the build so that
debug information, object files, and profile data remain available for
post-build inspection (callgrind symbol resolution, disassembly, etc.) until
the OS eventually reclaims it.

A symlink at the output path is updated to point at the new binary.

Examples
--------
# Build current checkout
  scripts/build_pgo_lto.py -o pycomparse-current

# Build main branch for comparison
  scripts/build_pgo_lto.py main -o pycomparse-baseline
"""

import argparse
import os
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

CPYTHON_LIB = Path.home() / "compiling" / "cpython-3.8" / "Lib"
TRAIN_FILES_CPYTHON = [
    "pydoc_data/topics.py",
    "_pydecimal.py",
    "_pyio.py",
    "argparse.py",
    "datetime.py",
    "doctest.py",
    "inspect.py",
    "tarfile.py",
]


# ── Helpers ───────────────────────────────────────────────────────────────────


def step(n: int, total: int, msg: str) -> None:
    print(f"\n[{n}/{total}] {msg}", flush=True)


def run(cmd: list, **kwargs) -> None:
    print(f"  $ {' '.join(str(c) for c in cmd)}", flush=True)
    subprocess.run([str(c) for c in cmd], check=True, **kwargs)


def cmake_cache_value(build_dir: Path, key: str) -> str | None:
    cache = build_dir / "CMakeCache.txt"
    if not cache.exists():
        return None
    for line in cache.read_text().splitlines():
        m = re.match(rf"^{re.escape(key)}(?::[^=]*)?\s*=\s*(.*)$", line)
        if m:
            return m.group(1).strip()
    return None


def find_llvm_profdata(gen_dir: Path) -> str:
    """Locate llvm-profdata, preferring the one that ships with the compiler."""
    if env_val := os.environ.get("LLVM_PROFDATA"):
        return env_val

    compiler = cmake_cache_value(gen_dir, "CMAKE_C_COMPILER")
    if compiler:
        compiler_path = Path(compiler)
        candidate = compiler_path.parent / "llvm-profdata"
        if candidate.exists():
            return str(candidate)
        m = re.search(r"-(\d+)$", compiler_path.name)
        if m:
            versioned = compiler_path.parent / f"llvm-profdata-{m.group(1)}"
            if versioned.exists():
                return str(versioned)

    found = shutil.which("llvm-profdata")
    if found:
        return found
    print("Error: llvm-profdata not found. Set LLVM_PROFDATA env var.", file=sys.stderr)
    sys.exit(1)


def check_clang(gen_dir: Path) -> None:
    compiler = cmake_cache_value(gen_dir, "CMAKE_C_COMPILER")
    if compiler and "clang" not in Path(compiler).name:
        print(f"Error: PGO requires Clang; found {compiler}", file=sys.stderr)
        sys.exit(1)


def cmake_configure(src_dir: Path, build_dir: Path, extra: list) -> None:
    run(
        [
            "cmake",
            "-S",
            src_dir,
            "-B",
            build_dir,
            "-G",
            "Ninja",
            "-DCMAKE_BUILD_TYPE=Release",
            "-DPYCOMPARSE_ENABLE_LTO=ON",
            *extra,
        ]
    )


def cmake_build(build_dir: Path, jobs: int) -> None:
    run(["cmake", "--build", build_dir, "-j", str(jobs)])


def run_training(
    binary: Path, train_cmd: str | None, profraw_dir: Path, src_dir: Path
) -> None:
    env = os.environ.copy()
    env["LLVM_PROFILE_FILE"] = str(profraw_dir / "pycomparse-%p.profraw")

    if train_cmd:
        subprocess.run(train_cmd, shell=True, check=True, env=env)
        return

    if CPYTHON_LIB.is_dir():
        files = [CPYTHON_LIB / f for f in TRAIN_FILES_CPYTHON]
    else:
        files = sorted((src_dir / "test").glob("*.py"))

    for _ in range(3):
        for f in files:
            subprocess.run(
                [str(binary), "--out", "/dev/null", str(f)],
                check=True,
                env=env,
            )


# ── Main ──────────────────────────────────────────────────────────────────────


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "revision",
        nargs="?",
        help="Git revision to build (default: current working tree)",
    )
    parser.add_argument(
        "-o",
        "--output",
        required=True,
        metavar="PATH",
        help="Symlink to create pointing at the built binary",
    )
    parser.add_argument(
        "--train-cmd",
        metavar="CMD",
        help="Shell command for the training workload",
    )
    parser.add_argument(
        "-j",
        "--jobs",
        type=int,
        default=os.cpu_count() or 4,
        help="Parallel build jobs (default: CPU count)",
    )
    args = parser.parse_args()

    output = Path(args.output)
    if not output.is_absolute():
        output = REPO_ROOT / output

    safe = re.sub(r"[^A-Za-z0-9._-]", "_", args.revision or "current")
    build_dir = Path(tempfile.mkdtemp(prefix=f"pycomparse-pgo-{safe}-"))

    gen_dir = build_dir / "gen"
    final_dir = build_dir / "final"
    profraw_dir = build_dir / "profraw"
    profdata = build_dir / "pycomparse.profdata"
    profraw_dir.mkdir()

    print(f"Build directory: {build_dir}", flush=True)

    # ── Source directory ──────────────────────────────────────────────────────
    if args.revision:
        src_dir = Path(tempfile.mkdtemp(prefix="pycomparse-src-"))
        print(f"\n[prep] Exporting {args.revision!r} → {src_dir}", flush=True)
        archive = subprocess.Popen(
            ["git", "-C", str(REPO_ROOT), "archive", args.revision],
            stdout=subprocess.PIPE,
        )
        subprocess.run(
            ["tar", "-x", "-C", str(src_dir)], stdin=archive.stdout, check=True
        )
        if archive.wait() != 0:
            print("Error: git archive failed.", file=sys.stderr)
            sys.exit(1)
    else:
        src_dir = REPO_ROOT

    # ── 1: configure instrumented build ──────────────────────────────────────
    step(1, 5, "Configuring instrumented PGO build")
    cmake_configure(src_dir, gen_dir, ["-DPYCOMPARSE_PGO_MODE=generate"])
    check_clang(gen_dir)

    # ── 2: build instrumented binary ──────────────────────────────────────────
    step(2, 5, "Building instrumented binary")
    cmake_build(gen_dir, args.jobs)

    # ── 3: training ───────────────────────────────────────────────────────────
    step(3, 5, "Running training workload")
    run_training(gen_dir / "pycomparse", args.train_cmd, profraw_dir, src_dir)

    profraw_files = list(profraw_dir.glob("*.profraw"))
    if not profraw_files:
        print("Error: no .profraw files generated.", file=sys.stderr)
        sys.exit(1)

    # ── 4: merge profile data ─────────────────────────────────────────────────
    step(4, 5, f"Merging {len(profraw_files)} profile file(s)")
    profdata_bin = find_llvm_profdata(gen_dir)
    run([profdata_bin, "merge", f"-output={profdata}", *profraw_files])

    # ── 5: build optimised binary ─────────────────────────────────────────────
    step(5, 5, "Building optimised PGO+LTO binary")
    cmake_configure(
        src_dir,
        final_dir,
        [
            "-DPYCOMPARSE_PGO_MODE=use",
            f"-DPYCOMPARSE_PGO_DATA={profdata}",
        ],
    )
    cmake_build(final_dir, args.jobs)

    # ── Symlink ───────────────────────────────────────────────────────────────
    binary = final_dir / "pycomparse"
    if output.is_symlink() or output.exists():
        output.unlink()
    output.symlink_to(binary)

    revision_label = args.revision or "current tree"
    print(f"\nDone ({revision_label})", flush=True)
    print(f"  artifacts: {build_dir}")
    print(f"  symlink:   {output} -> {binary}")


if __name__ == "__main__":
    main()
