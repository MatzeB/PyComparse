#!/usr/bin/env python3
"""Benchmark CPython vs pycomparse compile performance on Python sources."""

from __future__ import annotations

import argparse
import json
import os
import shlex
import shutil
import statistics
import subprocess
import sys
import time
from pathlib import Path
from typing import Any


COMPILE_WITH_CPYTHON = r"""
import py_compile
import sys

py_compile.compile(sys.argv[1], cfile=sys.argv[2], doraise=True)
"""


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Benchmark compile speed of CPython (uv run python) vs pycomparse. "
            "By default, benchmarks the top N largest files in cpython-3.8/Lib."
        )
    )
    parser.add_argument(
        "sources",
        nargs="*",
        help="Explicit .py files to benchmark. If omitted, select from --lib-root.",
    )
    parser.add_argument(
        "--lib-root",
        default="cpython-3.8/Lib",
        help="CPython Lib root used when no explicit sources are provided.",
    )
    parser.add_argument(
        "--top-n",
        type=int,
        default=20,
        help="Number of largest files to benchmark from --lib-root (default: 20).",
    )
    parser.add_argument(
        "--parser-test",
        default="build-release/parser_test",
        help="Path to pycomparse parser executable (default: build-release/parser_test).",
    )
    parser.add_argument(
        "--cpython-cmd",
        default="uv run python",
        help='Reference CPython command (default: "uv run python").',
    )
    parser.add_argument(
        "--warmup",
        type=int,
        default=2,
        help="Warmup runs per engine per file (default: 2).",
    )
    parser.add_argument(
        "--repeats",
        type=int,
        default=20,
        help="Measured runs per engine per file (default: 20).",
    )
    parser.add_argument(
        "--work-dir",
        default="/tmp/pycomparse-bench",
        help="Directory for generated .pyc outputs.",
    )
    parser.add_argument(
        "--json-out",
        default="",
        help="Optional path to write structured benchmark results as JSON.",
    )
    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="Abort immediately when one file fails in either compiler.",
    )
    parser.add_argument(
        "--runner",
        choices=("auto", "hyperfine", "internal"),
        default="auto",
        help=(
            "Timing backend. 'auto' uses hyperfine if installed, "
            "otherwise internal timer (default: auto)."
        ),
    )
    return parser.parse_args()


def cpython_env(cpython_cmd: list[str]) -> dict[str, str]:
    env = os.environ.copy()
    if len(cpython_cmd) >= 2 and cpython_cmd[0] == "uv" and cpython_cmd[1] == "run":
        env.setdefault("UV_CACHE_DIR", "/tmp/.uvcache")
    return env


def find_top_n_largest_py_files(lib_root: Path, top_n: int) -> list[Path]:
    candidates: list[Path] = []
    for path in lib_root.rglob("*.py"):
        if path.is_file():
            candidates.append(path)
    candidates.sort(key=lambda p: p.stat().st_size, reverse=True)
    return candidates[:top_n]


def run_cpython_compile(
    cpython_cmd: list[str], env: dict[str, str], source: Path, out_pyc: Path
) -> subprocess.CompletedProcess[bytes]:
    cmd = cpython_cmd + ["-c", COMPILE_WITH_CPYTHON, str(source), str(out_pyc)]
    return subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env)


def run_pycomparse_compile(source: Path, out_pyc: Path, parser_test: Path) -> int:
    with out_pyc.open("wb") as out_fp:
        proc = subprocess.run(
            [str(parser_test), str(source)], stdout=out_fp, stderr=subprocess.PIPE
        )
    return proc.returncode


def shell_join(parts: list[str]) -> str:
    return " ".join(shlex.quote(part) for part in parts)


def benchmark_with_hyperfine(
    *,
    engine: str,
    source: Path,
    out_pyc: Path,
    parser_test: Path,
    cpython_cmd: list[str],
    cpython_env_vars: dict[str, str],
    warmup: int,
    repeats: int,
    work_dir: Path,
) -> tuple[dict[str, Any] | None, str | None]:
    if engine == "cpython":
        preflight = run_cpython_compile(cpython_cmd, cpython_env_vars, source, out_pyc)
        if preflight.returncode != 0:
            return (
                None,
                preflight.stderr.decode("utf-8", errors="replace"),
            )
        cmd = shell_join(
            cpython_cmd + ["-c", COMPILE_WITH_CPYTHON, str(source), str(out_pyc)]
        )
    else:
        code = run_pycomparse_compile(source, out_pyc, parser_test)
        if code != 0:
            return None, f"pycomparse exited with code {code}"
        cmd = f"{shlex.quote(str(parser_test))} {shlex.quote(str(source))} > {shlex.quote(str(out_pyc))}"

    safe_name = str(source).replace("/", "_")
    export_json = work_dir / f"hyperfine_{engine}_{safe_name}.json"
    hyperfine_cmd = [
        "hyperfine",
        "--warmup",
        str(warmup),
        "--runs",
        str(repeats),
        "--export-json",
        str(export_json),
        cmd,
    ]
    proc = subprocess.run(
        hyperfine_cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=cpython_env_vars,
    )
    if proc.returncode != 0:
        stderr_text = proc.stderr.decode("utf-8", errors="replace")
        stdout_text = proc.stdout.decode("utf-8", errors="replace")
        return None, (stderr_text + "\n" + stdout_text).strip()

    try:
        payload = json.loads(export_json.read_text())
        entry = payload["results"][0]
        times_s = entry["times"]
    except (OSError, ValueError, KeyError, IndexError, TypeError) as err:
        return None, f"failed to parse hyperfine output: {err}"

    samples_ms = [float(t) * 1000.0 for t in times_s]
    summary = {
        "min_ms": float(entry["min"]) * 1000.0,
        "median_ms": float(entry["median"]) * 1000.0,
        "mean_ms": float(entry["mean"]) * 1000.0,
        "max_ms": float(entry["max"]) * 1000.0,
        "samples_ms": samples_ms,
    }
    return summary, None


def time_one_run(
    engine: str,
    source: Path,
    out_pyc: Path,
    parser_test: Path,
    cpython_cmd: list[str],
    cpython_env_vars: dict[str, str],
) -> tuple[float, str | None]:
    start = time.perf_counter_ns()

    if engine == "cpython":
        proc = run_cpython_compile(cpython_cmd, cpython_env_vars, source, out_pyc)
        ok = proc.returncode == 0
        err = proc.stderr.decode("utf-8", errors="replace") if not ok else None
    else:
        code = run_pycomparse_compile(source, out_pyc, parser_test)
        ok = code == 0
        err = f"pycomparse exited with code {code}" if not ok else None

    elapsed_ms = (time.perf_counter_ns() - start) / 1_000_000.0
    return elapsed_ms, err


def summarize_samples(samples: list[float]) -> dict[str, float]:
    return {
        "min_ms": min(samples),
        "median_ms": statistics.median(samples),
        "mean_ms": statistics.mean(samples),
        "max_ms": max(samples),
    }


def print_file_row(
    rel_file: str,
    size_bytes: int,
    cpython_median_ms: float,
    pycomparse_median_ms: float,
) -> None:
    ratio = cpython_median_ms / pycomparse_median_ms
    print(
        f"{rel_file:58} {size_bytes:10d} "
        f"{cpython_median_ms:12.3f} {pycomparse_median_ms:14.3f} {ratio:11.3f}"
    )


def main() -> int:
    args = parse_args()

    if args.warmup < 0 or args.repeats <= 0:
        print("error: --warmup must be >= 0 and --repeats must be > 0", file=sys.stderr)
        return 2

    parser_test = Path(args.parser_test)
    if not parser_test.is_file():
        print(f"error: parser executable not found: {parser_test}", file=sys.stderr)
        return 2

    cpython_cmd = shlex.split(args.cpython_cmd)
    if not cpython_cmd:
        print("error: --cpython-cmd is empty", file=sys.stderr)
        return 2
    cpython_env_vars = cpython_env(cpython_cmd)
    hyperfine_path = shutil.which("hyperfine")
    if args.runner == "hyperfine" and hyperfine_path is None:
        print("error: --runner hyperfine requested but hyperfine is not installed")
        return 2
    if args.runner == "auto":
        runner = "hyperfine" if hyperfine_path is not None else "internal"
    else:
        runner = args.runner

    if args.sources:
        sources = [Path(s) for s in args.sources]
    else:
        lib_root = Path(args.lib_root)
        if not lib_root.is_dir():
            print(f"error: --lib-root does not exist: {lib_root}", file=sys.stderr)
            return 2
        sources = find_top_n_largest_py_files(lib_root, args.top_n)

    missing = [s for s in sources if not s.is_file()]
    if missing:
        for path in missing:
            print(f"error: source file not found: {path}", file=sys.stderr)
        return 2

    if not sources:
        print("error: no source files selected", file=sys.stderr)
        return 2

    work_dir = Path(args.work_dir)
    work_dir.mkdir(parents=True, exist_ok=True)

    print("Benchmark configuration:")
    print(f"  parser_test: {parser_test}")
    print(f"  cpython_cmd: {' '.join(cpython_cmd)}")
    print(f"  warmup:      {args.warmup}")
    print(f"  repeats:     {args.repeats}")
    print(f"  runner:      {runner}")
    print(f"  files:       {len(sources)}")
    print()

    header = (
        f"{'file':58} {'size(bytes)':>10} {'cpython(ms)':>12} "
        f"{'pycomparse(ms)':>14} {'ratio':>11}"
    )
    print(header)
    print("-" * len(header))

    results: list[dict[str, Any]] = []
    failed = 0

    for source in sources:
        size_bytes = source.stat().st_size
        safe_name = str(source).replace("/", "_")
        ref_pyc = work_dir / f"cpython{safe_name}.pyc"
        pyc_pyc = work_dir / f"pycomparse{safe_name}.pyc"

        file_result: dict[str, Any] = {
            "file": str(source),
            "size_bytes": size_bytes,
            "cpython": {},
            "pycomparse": {},
            "status": "ok",
        }

        for engine, out_pyc in (("cpython", ref_pyc), ("pycomparse", pyc_pyc)):
            if runner == "hyperfine":
                summary, err = benchmark_with_hyperfine(
                    engine=engine,
                    source=source,
                    out_pyc=out_pyc,
                    parser_test=parser_test,
                    cpython_cmd=cpython_cmd,
                    cpython_env_vars=cpython_env_vars,
                    warmup=args.warmup,
                    repeats=args.repeats,
                    work_dir=work_dir,
                )
                if err is not None:
                    file_result[engine] = {"error": err}
                    file_result["status"] = "failed"
                    break
                assert summary is not None
                file_result[engine] = summary
            else:
                for _ in range(args.warmup):
                    _, err = time_one_run(
                        engine,
                        source,
                        out_pyc,
                        parser_test,
                        cpython_cmd,
                        cpython_env_vars,
                    )
                    if err is not None:
                        file_result[engine] = {"error": err}
                        file_result["status"] = "failed"
                        break
                if file_result["status"] == "failed":
                    break

                samples: list[float] = []
                for _ in range(args.repeats):
                    elapsed_ms, err = time_one_run(
                        engine,
                        source,
                        out_pyc,
                        parser_test,
                        cpython_cmd,
                        cpython_env_vars,
                    )
                    if err is not None:
                        file_result[engine] = {"error": err}
                        file_result["status"] = "failed"
                        break
                    samples.append(elapsed_ms)

                if file_result["status"] == "failed":
                    break

                summary = summarize_samples(samples)
                summary["samples_ms"] = samples
                file_result[engine] = summary

        if file_result["status"] == "ok":
            cpython_median = file_result["cpython"]["median_ms"]
            pycomparse_median = file_result["pycomparse"]["median_ms"]
            print_file_row(str(source), size_bytes, cpython_median, pycomparse_median)
            file_result["ratio_cpython_over_pycomparse"] = (
                cpython_median / pycomparse_median
            )
        else:
            failed += 1
            print(f"{str(source):58} {'-':>10} {'FAILED':>12} {'FAILED':>14} {'-':>11}")
            if args.fail_fast:
                results.append(file_result)
                break

        results.append(file_result)

    ok_results = [r for r in results if r.get("status") == "ok"]
    if ok_results:
        total_cpython = sum(r["cpython"]["median_ms"] for r in ok_results)
        total_pycomparse = sum(r["pycomparse"]["median_ms"] for r in ok_results)
        print()
        print("Totals (sum of per-file medians):")
        print(f"  cpython:    {total_cpython:.3f} ms")
        print(f"  pycomparse: {total_pycomparse:.3f} ms")
        print(f"  ratio:      {total_cpython / total_pycomparse:.3f}x")

    if args.json_out:
        payload: dict[str, Any] = {
            "config": {
                "parser_test": str(parser_test),
                "cpython_cmd": cpython_cmd,
                "warmup": args.warmup,
                "repeats": args.repeats,
                "work_dir": str(work_dir),
                "runner": runner,
                "hyperfine_path": hyperfine_path or "",
            },
            "results": results,
        }
        Path(args.json_out).write_text(json.dumps(payload, indent=2) + "\n")
        print(f"\nWrote JSON results to: {args.json_out}")

    if failed > 0:
        print(f"\nCompleted with {failed} failing file(s).", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
