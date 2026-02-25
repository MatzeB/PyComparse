#!/usr/bin/env python3
"""Build and run the libFuzzer harness for pycomparse."""

from __future__ import annotations

import argparse
import os
import subprocess
import sys


def get_nproc() -> int:
    try:
        return os.cpu_count() or 4
    except Exception:
        return 4


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Build and run the pycomparse fuzzer.",
        epilog=(
            "Extra arguments after -- are passed directly to libFuzzer "
            "(e.g. -max_total_time=60)."
        ),
    )
    parser.add_argument(
        "--build-dir",
        default="build-fuzz",
        help="Fuzzer build directory (default: build-fuzz)",
    )
    parser.add_argument(
        "--clang",
        default=os.environ.get("CLANG", "clang"),
        help="Clang binary to use (default: $CLANG or clang)",
    )
    parser.add_argument(
        "--jobs",
        type=int,
        default=int(os.environ.get("JOBS", get_nproc())),
        help="Parallel build jobs (default: $JOBS or nproc)",
    )

    args, extra = parser.parse_known_args()
    build_dir = args.build_dir
    repo_root = os.getcwd()
    build_dir_abs = os.path.abspath(build_dir)

    print(f"[1/2] Configuring fuzzer build in {build_dir}")
    subprocess.run(
        [
            "cmake",
            "-S", ".",
            "-B", build_dir,
            "-G", "Ninja",
            "-DCMAKE_BUILD_TYPE=RelWithDebInfo",
            f"-DCMAKE_C_COMPILER={args.clang}",
            "-DPYCOMPARSE_ENABLE_FUZZER=ON",
        ],
        check=True,
    )

    print("[2/2] Building fuzz_compile")
    subprocess.run(
        [
            "cmake",
            "--build", build_dir,
            "--target", "fuzz_compile",
            "-j", str(args.jobs),
        ],
        check=True,
    )

    os.makedirs(f"{build_dir}/corpus", exist_ok=True)
    os.makedirs(f"{build_dir}/artifacts", exist_ok=True)

    nproc = get_nproc()
    cmd = [
        os.path.join(build_dir_abs, "fuzz_compile"),
        os.path.join(build_dir_abs, "corpus"),
        os.path.join(repo_root, "test"),
        os.path.join(repo_root, "test/errors"),
        os.path.join(repo_root, "test/compile_only"),
        f"-artifact_prefix={os.path.join(build_dir_abs, 'artifacts')}/",
        f"-workers={nproc}",
        f"-jobs={nproc}",
        "-timeout=5",
        *extra,
    ]

    print("Running fuzzer (Ctrl-C to stop)...")
    os.chdir(build_dir_abs)
    os.execvp(cmd[0], cmd)
    return 1  # unreachable


if __name__ == "__main__":
    raise SystemExit(main())
