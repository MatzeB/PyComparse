#!/usr/bin/env python3
"""Benchmark CPython vs pycomparse compile performance on Python sources."""

from __future__ import annotations

import argparse
import csv
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

FILE_COL_WIDTH = 43


def commandline_option_provided(option: str) -> bool:
    for arg in sys.argv[1:]:
        if arg == option or arg.startswith(option + "="):
            return True
    return False


def parse_cpu_set_spec(spec: str) -> set[int]:
    cpus: set[int] = set()
    for raw_part in spec.split(","):
        part = raw_part.strip()
        if not part:
            raise ValueError(f"invalid CPU set: {spec!r}")
        if "-" in part:
            begin_s, end_s = part.split("-", 1)
            if not begin_s.isdigit() or not end_s.isdigit():
                raise ValueError(f"invalid CPU range: {part!r}")
            begin = int(begin_s)
            end = int(end_s)
            if end < begin:
                raise ValueError(f"invalid CPU range (end < begin): {part!r}")
            cpus.update(range(begin, end + 1))
            continue
        if not part.isdigit():
            raise ValueError(f"invalid CPU id: {part!r}")
        cpus.add(int(part))
    if not cpus:
        raise ValueError(f"invalid CPU set: {spec!r}")
    return cpus


def parse_lscpu_online_core_maxmhz() -> list[tuple[int, int | None, float]]:
    try:
        output = subprocess.check_output(
            ["lscpu", "-p=CPU,ONLINE,CORE,MAXMHZ"],
            stderr=subprocess.DEVNULL,
            text=True,
        )
    except (OSError, subprocess.CalledProcessError):
        return []

    result: list[tuple[int, int | None, float]] = []
    for line in output.splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        parts = [p.strip() for p in line.split(",")]
        if len(parts) < 4:
            continue
        cpu_s, online_s, core_s, maxmhz_s = parts[0], parts[1], parts[2], parts[3]
        if not cpu_s.isdigit():
            continue
        cpu = int(cpu_s)
        online = online_s.lower() in {"y", "yes", "1", "true"}
        if not online:
            continue
        try:
            max_mhz = float(maxmhz_s)
        except ValueError:
            max_mhz = 0.0
        try:
            core = int(core_s)
        except ValueError:
            core = None
        result.append((cpu, core, max_mhz))
    return result


def choose_auto_affinity_cpu(allowed_cpus: set[int]) -> int:
    assert allowed_cpus
    cpu_info = [
        (cpu, core, max_mhz)
        for cpu, core, max_mhz in parse_lscpu_online_core_maxmhz()
        if cpu in allowed_cpus
    ]
    if cpu_info:
        max_mhz = max(max_mhz for _, _, max_mhz in cpu_info)
        fastest = [
            (cpu, core)
            for cpu, core, mhz in cpu_info
            if mhz >= max_mhz - 1e-6
        ]
        cpu0_core = None
        for cpu, core in fastest:
            if cpu == 0:
                cpu0_core = core
                break
        if cpu0_core is None:
            for cpu, core, _ in cpu_info:
                if cpu == 0:
                    cpu0_core = core
                    break

        core_to_primary_cpu: dict[int | None, int] = {}
        for cpu, core, _ in cpu_info:
            current = core_to_primary_cpu.get(core)
            if current is None or cpu < current:
                core_to_primary_cpu[core] = cpu

        def sort_key(item: tuple[int, int | None]) -> tuple[int, int, int, int]:
            cpu, core = item
            shares_cpu0_core = int(cpu0_core is not None and core == cpu0_core)
            primary_cpu = core_to_primary_cpu.get(core, cpu)
            non_primary = int(cpu != primary_cpu)
            is_cpu0 = int(cpu == 0)
            return (shares_cpu0_core, non_primary, is_cpu0, cpu)

        fastest.sort(key=sort_key)
        return fastest[0][0]

    sorted_allowed = sorted(allowed_cpus)
    for cpu in sorted_allowed:
        if cpu != 0:
            return cpu
    return sorted_allowed[0]


def configure_cpu_affinity(spec: str) -> tuple[str, str | None]:
    normalized = spec.strip().lower()
    if normalized in {"off", "none", "disable", "disabled"}:
        return "off", None

    if not hasattr(os, "sched_setaffinity") or not hasattr(os, "sched_getaffinity"):
        return "off", "CPU affinity is unsupported on this platform; continuing unpinned"

    try:
        allowed_cpus = set(os.sched_getaffinity(0))
    except OSError as err:
        return "off", f"could not query current CPU affinity: {err}"
    if not allowed_cpus:
        return "off", "no allowed CPUs in current affinity mask; continuing unpinned"

    if normalized == "auto":
        cpus = {choose_auto_affinity_cpu(allowed_cpus)}
        mode = "auto"
    else:
        try:
            cpus = parse_cpu_set_spec(spec)
        except ValueError as err:
            return "off", str(err)
        if not cpus.issubset(allowed_cpus):
            unavailable = sorted(cpus - allowed_cpus)
            return (
                "off",
                f"requested CPUs not allowed in current affinity mask: {unavailable}",
            )
        mode = "manual"

    try:
        os.sched_setaffinity(0, cpus)
    except OSError as err:
        return "off", f"failed to set CPU affinity to {sorted(cpus)}: {err}"
    return f"{mode}:{','.join(str(cpu) for cpu in sorted(cpus))}", None


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
        "--tested",
        "--parser-test",
        dest="tested",
        default="build-release/pycomparse",
        help=(
            "Path to tested compiler executable "
            "(default: build-release/pycomparse). "
            "Alias: --parser-test."
        ),
    )
    parser.add_argument(
        "--baseline",
        "--cpython-cmd",
        dest="baseline",
        default="uv run python",
        help='Baseline compiler command (default: "uv run python"). Alias: --cpython-cmd.',
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
        "--tsv-out",
        default="",
        help="Optional path to write per-file benchmark results as TSV.",
    )
    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="Abort immediately when one file fails in either compiler.",
    )
    parser.add_argument(
        "--runner",
        choices=("auto", "hyperfine", "internal", "callgrind"),
        default="auto",
        help=(
            "Timing backend. 'auto' uses hyperfine if installed, "
            "otherwise internal timer (default: auto). "
            "Use 'callgrind' for instruction-count benchmarking."
        ),
    )
    parser.add_argument(
        "--cpu-affinity",
        default="auto",
        help=(
            "CPU affinity for this benchmark process. Default: auto "
            "(pick one online high-frequency CPU). "
            "Use 'off' to disable, or pass a CPU set like '2' or '2,4-5'."
        ),
    )
    return parser.parse_args()


def cpython_env(cpython_cmd: list[str]) -> dict[str, str]:
    env = os.environ.copy()
    # Adjust for AI sandboxes blocking $HOME/.cache/uv
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
    proc = subprocess.run(
        [str(parser_test), "--out", str(out_pyc), str(source)],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
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
        cmd = (
            f"{shlex.quote(str(parser_test))} --out {shlex.quote(str(out_pyc))} "
            f"{shlex.quote(str(source))}"
        )

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
    mean_ms = float(entry["mean"]) * 1000.0
    stddev_ms = statistics.pstdev(samples_ms) if len(samples_ms) > 1 else 0.0
    summary = {
        "min_ms": float(entry["min"]) * 1000.0,
        "median_ms": float(entry["median"]) * 1000.0,
        "mean_ms": mean_ms,
        "max_ms": float(entry["max"]) * 1000.0,
        "stddev_ms": stddev_ms,
        "samples_ms": samples_ms,
    }
    return summary, None


def parse_callgrind_summary(callgrind_out: Path) -> tuple[int | None, str | None]:
    try:
        lines = callgrind_out.read_text(encoding="utf-8", errors="replace").splitlines()
    except OSError as err:
        return None, f"failed to read callgrind output {callgrind_out}: {err}"

    for line in lines:
        if not line.startswith("summary:"):
            continue
        value = line.split(":", 1)[1].strip()
        try:
            return int(value), None
        except ValueError:
            return None, f"invalid callgrind summary value in {callgrind_out}: {value!r}"

    return None, f"missing callgrind summary in {callgrind_out}"


def benchmark_with_callgrind(
    *,
    engine: str,
    source: Path,
    out_pyc: Path,
    parser_test: Path,
    cpython_cmd: list[str],
    cpython_env_vars: dict[str, str],
    work_dir: Path,
) -> tuple[dict[str, Any] | None, str | None]:
    safe_name = str(source).replace("/", "_")
    callgrind_out = work_dir / f"callgrind_{engine}_{safe_name}.out"
    if engine == "cpython":
        cmd = [
            "valgrind",
            "--tool=callgrind",
            f"--callgrind-out-file={callgrind_out}",
            *cpython_cmd,
            "-c",
            COMPILE_WITH_CPYTHON,
            str(source),
            str(out_pyc),
        ]
        env = cpython_env_vars
    else:
        cmd = [
            "valgrind",
            "--tool=callgrind",
            f"--callgrind-out-file={callgrind_out}",
            str(parser_test),
            "--out",
            str(out_pyc),
            str(source),
        ]
        env = None

    proc = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=env)
    if proc.returncode != 0:
        stderr_text = proc.stderr.decode("utf-8", errors="replace")
        stdout_text = proc.stdout.decode("utf-8", errors="replace")
        return None, (stderr_text + "\n" + stdout_text).strip()

    ir, err = parse_callgrind_summary(callgrind_out)
    if err is not None or ir is None:
        return None, err

    ir_value = float(ir)
    summary = {
        "min_ms": ir_value,
        "median_ms": ir_value,
        "mean_ms": ir_value,
        "max_ms": ir_value,
        "samples_ms": [ir_value],
        "unit": "Ir",
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


def safe_ratio(numerator: float, denominator: float) -> float | None:
    if denominator <= 0.0:
        return None
    return numerator / denominator


def format_plus_minus(summary: dict[str, Any]) -> str:
    stddev = summary.get("stddev_ms")
    if not isinstance(stddev, (int, float)):
        return "-"
    return f"±{float(stddev):.2f}ms"


def truncate_middle(text: str, width: int) -> str:
    if width <= 0:
        return ""
    if len(text) <= width:
        return text
    if width == 1:
        return "…"
    if width == 2:
        return text[:1] + "…"
    keep = width - 1
    head = (keep + 1) // 2
    tail = keep // 2
    return f"{text[:head]}…{text[-tail:]}"


def write_tsv(path: Path, results: list[dict[str, Any]], metric_unit: str = "ms") -> None:
    unit_suffix = metric_unit.lower()

    def metric(result: dict[str, Any], engine: str, key: str) -> str:
        value = result.get(engine, {}).get(key)
        if isinstance(value, (int, float)):
            return f"{float(value):.6f}"
        return ""

    def error_text(result: dict[str, Any], engine: str) -> str:
        value = result.get(engine, {}).get("error")
        if not isinstance(value, str):
            return ""
        return value.replace("\r", "\\r").replace("\n", "\\n")

    with path.open("w", encoding="utf-8", newline="") as fp:
        writer = csv.writer(fp, delimiter="\t")
        writer.writerow(
            [
                "file",
                "size_bytes",
                "status",
                f"cpython_min_{unit_suffix}",
                f"cpython_median_{unit_suffix}",
                f"cpython_mean_{unit_suffix}",
                f"cpython_max_{unit_suffix}",
                f"pycomparse_min_{unit_suffix}",
                f"pycomparse_median_{unit_suffix}",
                f"pycomparse_mean_{unit_suffix}",
                f"pycomparse_max_{unit_suffix}",
                "ratio_cpython_over_pycomparse",
                "cpython_error",
                "pycomparse_error",
            ]
        )
        for result in results:
            ratio = result.get("ratio_cpython_over_pycomparse")
            if isinstance(ratio, (int, float)):
                ratio_text = f"{float(ratio):.6f}"
            else:
                ratio_text = ""
            writer.writerow(
                [
                    result.get("file", ""),
                    result.get("size_bytes", ""),
                    result.get("status", ""),
                    metric(result, "cpython", "min_ms"),
                    metric(result, "cpython", "median_ms"),
                    metric(result, "cpython", "mean_ms"),
                    metric(result, "cpython", "max_ms"),
                    metric(result, "pycomparse", "min_ms"),
                    metric(result, "pycomparse", "median_ms"),
                    metric(result, "pycomparse", "mean_ms"),
                    metric(result, "pycomparse", "max_ms"),
                    ratio_text,
                    error_text(result, "cpython"),
                    error_text(result, "pycomparse"),
                ]
            )


def print_file_row(
    rel_file: str,
    size_bytes: int,
    cpython_median_ms: float,
    pycomparse_median_ms: float,
    baseline_plus_minus: str | None = None,
    tested_plus_minus: str | None = None,
) -> None:
    ratio = safe_ratio(cpython_median_ms, pycomparse_median_ms)
    ratio_text = "inf" if ratio is None else f"{ratio:.2f}"
    file_display = truncate_middle(rel_file, FILE_COL_WIDTH)
    row = f"{file_display:{FILE_COL_WIDTH}} {size_bytes:>12,} {cpython_median_ms:12.2f}"
    if baseline_plus_minus is not None and tested_plus_minus is not None:
        row += f" {baseline_plus_minus:>9}"
    row += f" {pycomparse_median_ms:14.2f}"
    if baseline_plus_minus is not None and tested_plus_minus is not None:
        row += f" {tested_plus_minus:>9}"
    row += f" {ratio_text:>11}"
    print(row)


def print_file_row_callgrind(
    rel_file: str, size_bytes: int, cpython_ir: float, pycomparse_ir: float
) -> None:
    ratio = safe_ratio(cpython_ir, pycomparse_ir)
    ratio_text = "inf" if ratio is None else f"{ratio:.2f}"
    file_display = truncate_middle(rel_file, FILE_COL_WIDTH)
    row = (
        f"{file_display:{FILE_COL_WIDTH}} {size_bytes:>12,} "
        f"{int(cpython_ir):>12,} {int(pycomparse_ir):>14,} {ratio_text:>11}"
    )
    print(row)


def main() -> int:
    args = parse_args()

    parser_test = Path(args.tested)
    if not parser_test.is_file():
        print(f"error: tested executable not found: {parser_test}", file=sys.stderr)
        return 2

    cpython_cmd = shlex.split(args.baseline)
    if not cpython_cmd:
        print("error: --baseline is empty", file=sys.stderr)
        return 2
    cpython_env_vars = cpython_env(cpython_cmd)
    hyperfine_path = shutil.which("hyperfine")
    callgrind_path = shutil.which("valgrind")
    if args.runner == "hyperfine" and hyperfine_path is None:
        print("error: --runner hyperfine requested but hyperfine is not installed")
        return 2
    if args.runner == "auto":
        runner = "hyperfine" if hyperfine_path is not None else "internal"
    else:
        runner = args.runner
    if runner == "callgrind" and callgrind_path is None:
        print("error: --runner callgrind requested but valgrind is not installed")
        return 2

    affinity_request = args.cpu_affinity.strip().lower()
    manual_affinity_requested = affinity_request not in {
        "auto",
        "off",
        "none",
        "disable",
        "disabled",
    }
    affinity, affinity_warning = configure_cpu_affinity(args.cpu_affinity)
    if affinity_warning is not None:
        if manual_affinity_requested:
            print(f"error: {affinity_warning}", file=sys.stderr)
            return 2
        print(f"warning: {affinity_warning}", file=sys.stderr)

    effective_warmup = args.warmup
    effective_repeats = args.repeats
    if runner == "callgrind":
        effective_warmup = 0
        effective_repeats = 1
        if commandline_option_provided("--warmup"):
            print(
                "warning: --warmup is ignored with --runner callgrind",
                file=sys.stderr,
            )
        if commandline_option_provided("--repeats"):
            print(
                "warning: --repeats is ignored with --runner callgrind",
                file=sys.stderr,
            )
    elif args.warmup < 0 or args.repeats <= 0:
        print("error: --warmup must be >= 0 and --repeats must be > 0", file=sys.stderr)
        return 2

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
    print(f"  tested:      {parser_test}")
    print(f"  baseline:    {' '.join(cpython_cmd)}")
    print(f"  affinity:    {affinity}")
    if runner == "callgrind":
        print("  warmup:      ignored (callgrind)")
        print("  repeats:     ignored (callgrind)")
    else:
        print(f"  warmup:      {effective_warmup}")
        print(f"  repeats:     {effective_repeats}")
    print(f"  runner:      {runner}")
    print(f"  files:       {len(sources)}")
    print()

    if runner == "hyperfine":
        header = (
            f"{'file':{FILE_COL_WIDTH}} {'size(bytes)':>12} {'cpython(ms)':>12} {'':>9} "
            f"{'pycomparse(ms)':>14} {'':>9} {'speedup':>11}"
        )
    elif runner == "callgrind":
        header = (
            f"{'file':{FILE_COL_WIDTH}} {'size(bytes)':>12} {'cpython(Ir)':>12} "
            f"{'pycomparse(Ir)':>14} {'speedup':>11}"
        )
    else:
        header = (
            f"{'file':{FILE_COL_WIDTH}} {'size(bytes)':>12} {'cpython(ms)':>12} "
            f"{'pycomparse(ms)':>14} {'speedup':>11}"
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
                    warmup=effective_warmup,
                    repeats=effective_repeats,
                    work_dir=work_dir,
                )
                if err is not None:
                    file_result[engine] = {"error": err}
                    file_result["status"] = "failed"
                    break
                assert summary is not None
                file_result[engine] = summary
            elif runner == "callgrind":
                summary, err = benchmark_with_callgrind(
                    engine=engine,
                    source=source,
                    out_pyc=out_pyc,
                    parser_test=parser_test,
                    cpython_cmd=cpython_cmd,
                    cpython_env_vars=cpython_env_vars,
                    work_dir=work_dir,
                )
                if err is not None:
                    file_result[engine] = {"error": err}
                    file_result["status"] = "failed"
                    break
                assert summary is not None
                file_result[engine] = summary
            else:
                for _ in range(effective_warmup):
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
                for _ in range(effective_repeats):
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
            baseline_plus_minus = None
            tested_plus_minus = None
            if runner == "hyperfine":
                baseline_plus_minus = format_plus_minus(file_result["cpython"])
                tested_plus_minus = format_plus_minus(file_result["pycomparse"])
            if runner == "callgrind":
                print_file_row_callgrind(
                    str(source), size_bytes, cpython_median, pycomparse_median
                )
            else:
                print_file_row(
                    str(source),
                    size_bytes,
                    cpython_median,
                    pycomparse_median,
                    baseline_plus_minus=baseline_plus_minus,
                    tested_plus_minus=tested_plus_minus,
                )
            file_result["ratio_cpython_over_pycomparse"] = safe_ratio(
                cpython_median, pycomparse_median
            )
        else:
            failed += 1
            failed_file_display = truncate_middle(str(source), FILE_COL_WIDTH)
            failed_row = (
                f"{failed_file_display:{FILE_COL_WIDTH}} {'-':>12} {'FAILED':>12} "
                f"{'FAILED':>14} {'-':>11}"
            )
            if runner == "hyperfine":
                failed_row = (
                    f"{failed_file_display:{FILE_COL_WIDTH}} {'-':>12} {'FAILED':>12} {'-':>9} "
                    f"{'FAILED':>14} {'-':>9} {'-':>11}"
                )
            print(failed_row)
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
        if runner == "callgrind":
            print(f"  cpython:    {int(total_cpython):,} Ir")
            print(f"  pycomparse: {int(total_pycomparse):,} Ir")
        else:
            print(f"  cpython:    {total_cpython:.2f} ms")
            print(f"  pycomparse: {total_pycomparse:.2f} ms")
        total_ratio = safe_ratio(total_cpython, total_pycomparse)
        if total_ratio is None:
            print("  ratio:      infx")
        else:
            print(f"  ratio:      {total_ratio:.2f}x")

    if args.json_out:
        payload: dict[str, Any] = {
            "config": {
                "tested": str(parser_test),
                "baseline": cpython_cmd,
                "warmup": args.warmup,
                "repeats": args.repeats,
                "effective_warmup": effective_warmup,
                "effective_repeats": effective_repeats,
                "work_dir": str(work_dir),
                "runner": runner,
                "cpu_affinity": affinity,
                "cpu_affinity_request": args.cpu_affinity,
                "metric_unit": "Ir" if runner == "callgrind" else "ms",
                "hyperfine_path": hyperfine_path or "",
                "callgrind_path": callgrind_path or "",
            },
            "results": results,
        }
        Path(args.json_out).write_text(json.dumps(payload, indent=2) + "\n")
        print(f"\nWrote JSON results to: {args.json_out}")
    if args.tsv_out:
        write_tsv(
            Path(args.tsv_out), results, metric_unit="Ir" if runner == "callgrind" else "ms"
        )
        print(f"Wrote TSV results to: {args.tsv_out}")

    if failed > 0:
        print(f"\nCompleted with {failed} failing file(s).", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
