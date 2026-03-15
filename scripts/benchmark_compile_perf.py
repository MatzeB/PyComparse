#!/usr/bin/env python3
"""Benchmark compile performance across multiple compilers and runners."""

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
from dataclasses import dataclass
from pathlib import Path
from typing import Any


COMPILE_WITH_CPYTHON = r"""
import py_compile
import sys

py_compile.compile(sys.argv[1], cfile=sys.argv[2], doraise=True)
"""

FILE_COL_WIDTH = 43
METRIC_COL_MIN_WIDTH = 14


@dataclass(frozen=True)
class CompilerSpec:
    name: str
    compiler_kind: str
    python_cmd: list[str] | None = None
    compiler_path: Path | None = None
    env: dict[str, str] | None = None


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
        fastest = [(cpu, core) for cpu, core, mhz in cpu_info if mhz >= max_mhz - 1e-6]
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
        return (
            "off",
            "CPU affinity is unsupported on this platform; continuing unpinned",
        )

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
            "Benchmark compile speed across any number of compilers and runners. "
            "Benchmarks explicit sources or the top N largest files in cpython-3.8/Lib."
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
        "--compiler",
        action="append",
        default=[],
        metavar="NAME=PATH",
        help=(
            "Pycomparse compiler executable to benchmark. "
            "May be provided multiple times."
        ),
    )
    parser.add_argument(
        "--python-compiler",
        action="append",
        default=[],
        metavar="NAME=CMD",
        help=(
            "Python command to benchmark, for example "
            "--python-compiler python='uv run python'. "
            "May be provided multiple times."
        ),
    )
    parser.add_argument(
        "--hyperfine-warmup",
        type=int,
        default=2,
        help="Warmup runs per compiler per file for the hyperfine runner (default: 2).",
    )
    parser.add_argument(
        "--hyperfine-repeats",
        type=int,
        default=20,
        help="Measured runs per compiler per file for the hyperfine runner (default: 20).",
    )
    parser.add_argument(
        "--internal-warmup",
        type=int,
        default=2,
        help="Warmup runs per compiler per file for the internal runner (default: 2).",
    )
    parser.add_argument(
        "--internal-repeats",
        type=int,
        default=20,
        help="Measured runs per compiler per file for the internal runner (default: 20).",
    )
    parser.add_argument(
        "--hyperfine-arg",
        action="append",
        default=[],
        metavar="ARG",
        help=(
            "Extra argument passed through to hyperfine. "
            "May be provided multiple times; each value is shell-split."
        ),
    )
    parser.add_argument(
        "--callgrind-arg",
        action="append",
        default=[],
        metavar="ARG",
        help=(
            "Extra argument passed through to valgrind/callgrind. "
            "May be provided multiple times; each value is shell-split."
        ),
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
        help=(
            "Optional path to write per-file results as TSV. "
            "With multiple runners, one file per runner is written."
        ),
    )
    parser.add_argument(
        "--fail-fast",
        action="store_true",
        help="Abort immediately when one file fails in any compiler.",
    )
    parser.add_argument(
        "--runner",
        action="append",
        choices=("auto", "hyperfine", "internal", "callgrind"),
        default=[],
        help=(
            "Benchmark runner to use. May be provided multiple times. "
            "'auto' resolves to hyperfine when available, otherwise internal."
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


def python_cmd_env(python_cmd: list[str]) -> dict[str, str]:
    env = os.environ.copy()
    if len(python_cmd) >= 2 and python_cmd[0] == "uv" and python_cmd[1] == "run":
        env.setdefault("UV_CACHE_DIR", "/tmp/.uvcache")
    return env


def parse_named_value(raw: str, flag: str) -> tuple[str, str]:
    name, sep, value = raw.partition("=")
    if sep == "" or not name or not value:
        raise ValueError(f"{flag} expects NAME=VALUE, got {raw!r}")
    return name, value


def parse_passthrough_args(values: list[str], flag: str) -> list[str]:
    parts: list[str] = []
    for raw in values:
        split = shlex.split(raw)
        if not split:
            raise ValueError(f"{flag} received an empty argument")
        parts.extend(split)
    return parts


def build_compilers(args: argparse.Namespace) -> list[CompilerSpec]:
    compilers: list[CompilerSpec] = []
    seen_names: set[str] = set()

    def add(spec: CompilerSpec) -> None:
        if spec.name in seen_names:
            raise ValueError(f"duplicate compiler name: {spec.name!r}")
        seen_names.add(spec.name)
        compilers.append(spec)

    for raw in args.python_compiler:
        name, value = parse_named_value(raw, "--python-compiler")
        python_cmd = shlex.split(value)
        if not python_cmd:
            raise ValueError(f"--python-compiler for {name!r} is empty")
        add(
            CompilerSpec(
                name=name,
                compiler_kind="python",
                python_cmd=python_cmd,
                env=python_cmd_env(python_cmd),
            )
        )

    for raw in args.compiler:
        name, value = parse_named_value(raw, "--compiler")
        compiler_path = Path(value)
        if not compiler_path.is_file():
            raise ValueError(f"compiler not found for {name!r}: {compiler_path}")
        add(
            CompilerSpec(
                name=name,
                compiler_kind="pycomparse",
                compiler_path=compiler_path,
            )
        )

    if not compilers:
        raise ValueError(
            "at least one compiler is required; use --compiler or --python-compiler"
        )

    return compilers


def resolve_runners(requested: list[str]) -> tuple[list[str], str | None, str | None]:
    runner_requests = requested or ["auto"]
    hyperfine_path = shutil.which("hyperfine")
    callgrind_path = shutil.which("valgrind")

    resolved: list[str] = []
    seen: set[str] = set()
    for runner in runner_requests:
        actual = runner
        if runner == "auto":
            actual = "hyperfine" if hyperfine_path is not None else "internal"
        if actual == "hyperfine" and hyperfine_path is None:
            raise ValueError(
                "--runner hyperfine requested but hyperfine is not installed"
            )
        if actual == "callgrind" and callgrind_path is None:
            raise ValueError(
                "--runner callgrind requested but valgrind is not installed"
            )
        if actual not in seen:
            seen.add(actual)
            resolved.append(actual)

    return resolved, hyperfine_path, callgrind_path


def find_top_n_largest_py_files(lib_root: Path, top_n: int) -> list[Path]:
    candidates: list[Path] = []
    for path in lib_root.rglob("*.py"):
        if path.is_file():
            candidates.append(path)
    candidates.sort(key=lambda p: p.stat().st_size, reverse=True)
    return candidates[:top_n]


def compiler_compile_command(
    compiler: CompilerSpec, source: Path, out_pyc: Path
) -> list[str]:
    if compiler.compiler_kind == "python":
        assert compiler.python_cmd is not None
        return compiler.python_cmd + [
            "-c",
            COMPILE_WITH_CPYTHON,
            str(source),
            str(out_pyc),
        ]
    assert compiler.compiler_path is not None
    return [str(compiler.compiler_path), "--out", str(out_pyc), str(source)]


def run_compiler_compile(
    compiler: CompilerSpec, source: Path, out_pyc: Path
) -> subprocess.CompletedProcess[bytes]:
    return subprocess.run(
        compiler_compile_command(compiler, source, out_pyc),
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=compiler.env,
    )


def shell_join(parts: list[str]) -> str:
    return " ".join(shlex.quote(part) for part in parts)


def sanitize_name(text: str) -> str:
    return "".join(ch if ch.isalnum() or ch in {"-", "_", "."} else "_" for ch in text)


def benchmark_with_hyperfine(
    *,
    runner_name: str,
    compiler: CompilerSpec,
    source: Path,
    out_pyc: Path,
    warmup: int,
    repeats: int,
    extra_args: list[str],
    work_dir: Path,
) -> tuple[dict[str, Any] | None, str | None]:
    preflight = run_compiler_compile(compiler, source, out_pyc)
    if preflight.returncode != 0:
        stderr_text = preflight.stderr.decode("utf-8", errors="replace").strip()
        if not stderr_text:
            stderr_text = f"{compiler.name} exited with code {preflight.returncode}"
        return None, stderr_text

    cmd = shell_join(compiler_compile_command(compiler, source, out_pyc))
    safe_name = sanitize_name(str(source))
    export_json = work_dir / f"{runner_name}_{compiler.name}_{safe_name}.json"
    hyperfine_cmd = [
        "hyperfine",
        *extra_args,
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
        env=compiler.env,
    )
    if proc.returncode != 0:
        stderr_text = proc.stderr.decode("utf-8", errors="replace")
        stdout_text = proc.stdout.decode("utf-8", errors="replace")
        return None, (stderr_text + "\n" + stdout_text).strip()

    try:
        payload = json.loads(export_json.read_text())
        results = payload["results"]
        if not isinstance(results, list) or len(results) != 1:
            return None, (
                "hyperfine produced an unexpected number of benchmark results; "
                "extra positional commands are not supported via --hyperfine-arg"
            )
        entry = results[0]
        times_s = entry["times"]
    except (OSError, ValueError, KeyError, IndexError, TypeError) as err:
        return None, f"failed to parse hyperfine output: {err}"

    samples = [float(t) * 1000.0 for t in times_s]
    summary = {
        "min": float(entry["min"]) * 1000.0,
        "median": float(entry["median"]) * 1000.0,
        "mean": float(entry["mean"]) * 1000.0,
        "max": float(entry["max"]) * 1000.0,
        "stddev": statistics.pstdev(samples) if len(samples) > 1 else 0.0,
        "samples": samples,
        "unit": "ms",
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
            return (
                None,
                f"invalid callgrind summary value in {callgrind_out}: {value!r}",
            )

    return None, f"missing callgrind summary in {callgrind_out}"


def benchmark_with_callgrind(
    *,
    runner_name: str,
    compiler: CompilerSpec,
    source: Path,
    out_pyc: Path,
    extra_args: list[str],
    work_dir: Path,
) -> tuple[dict[str, Any] | None, str | None]:
    safe_name = sanitize_name(str(source))
    callgrind_out = work_dir / f"{runner_name}_{compiler.name}_{safe_name}.out"
    cmd = [
        "valgrind",
        *extra_args,
        "--tool=callgrind",
        f"--callgrind-out-file={callgrind_out}",
        *compiler_compile_command(compiler, source, out_pyc),
    ]

    proc = subprocess.run(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=compiler.env,
    )
    if proc.returncode != 0:
        stderr_text = proc.stderr.decode("utf-8", errors="replace")
        stdout_text = proc.stdout.decode("utf-8", errors="replace")
        return None, (stderr_text + "\n" + stdout_text).strip()

    ir, err = parse_callgrind_summary(callgrind_out)
    if err is not None or ir is None:
        return None, err

    ir_value = float(ir)
    return {
        "min": ir_value,
        "median": ir_value,
        "mean": ir_value,
        "max": ir_value,
        "samples": [ir_value],
        "unit": "Ir",
    }, None


def time_one_run(
    compiler: CompilerSpec,
    source: Path,
    out_pyc: Path,
) -> tuple[float, str | None]:
    start = time.perf_counter_ns()
    proc = run_compiler_compile(compiler, source, out_pyc)
    ok = proc.returncode == 0
    err = proc.stderr.decode("utf-8", errors="replace") if not ok else None
    if not ok and not err.strip():
        err = f"{compiler.name} exited with code {proc.returncode}"
    elapsed_ms = (time.perf_counter_ns() - start) / 1_000_000.0
    return elapsed_ms, err


def summarize_samples(samples: list[float], unit: str) -> dict[str, Any]:
    return {
        "min": min(samples),
        "median": statistics.median(samples),
        "mean": statistics.mean(samples),
        "max": max(samples),
        "samples": samples,
        "stddev": statistics.pstdev(samples) if len(samples) > 1 else 0.0,
        "unit": unit,
    }


def safe_ratio(numerator: float, denominator: float) -> float | None:
    if denominator <= 0.0:
        return None
    return numerator / denominator


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


def describe_compiler(spec: CompilerSpec) -> str:
    if spec.compiler_kind == "python":
        assert spec.python_cmd is not None
        return shell_join(spec.python_cmd)
    assert spec.compiler_path is not None
    return str(spec.compiler_path)


def output_path_for_runner(base_path: str, runner: str, runner_count: int) -> Path:
    path = Path(base_path)
    if runner_count == 1:
        return path
    suffix = "".join(path.suffixes)
    stem = path.name[: -len(suffix)] if suffix else path.name
    filename = f"{stem}.{runner}{suffix}"
    return path.with_name(filename)


def value_for_summary(summary: dict[str, Any]) -> float | None:
    value = summary.get("median")
    if isinstance(value, (int, float)):
        return float(value)
    return None


def format_metric(value: float, unit: str) -> str:
    if unit == "Ir":
        return f"{int(value):,}"
    return f"{value:.2f}"


def metric_columns(compilers: list[CompilerSpec], unit: str) -> list[tuple[str, int]]:
    columns: list[tuple[str, int]] = []
    for compiler in compilers:
        label = f"{compiler.name}({unit})"
        width = max(METRIC_COL_MIN_WIDTH, len(label))
        columns.append((label, width))
    return columns


def print_runner_header(
    runner: str,
    unit: str,
    compilers: list[CompilerSpec],
    warmup: int | None,
    repeats: int | None,
) -> None:
    print(f"Runner: {runner}")
    if warmup is not None:
        print(f"  warmup:  {warmup}")
    if repeats is not None:
        print(f"  repeats: {repeats}")
    columns = metric_columns(compilers, unit)
    header = f"{'file':{FILE_COL_WIDTH}} {'size(bytes)':>12}"
    for label, width in columns:
        header += f" {label:>{width}}"
    header += f" {'best':>12}"
    print(header)
    print("-" * len(header))


def print_file_result_row(
    source: Path,
    size_bytes: int,
    compilers: list[CompilerSpec],
    unit: str,
    compiler_results: dict[str, dict[str, Any]],
) -> None:
    columns = metric_columns(compilers, unit)
    values: list[tuple[str, float]] = []
    row = f"{truncate_middle(str(source), FILE_COL_WIDTH):{FILE_COL_WIDTH}} {size_bytes:>12,}"
    for compiler, (_, width) in zip(compilers, columns):
        summary = compiler_results.get(compiler.name, {})
        value = value_for_summary(summary)
        if value is None:
            row += f" {'FAILED':>{width}}"
            continue
        values.append((compiler.name, value))
        row += f" {format_metric(value, unit):>{width}}"

    if values:
        best_name, best_value = min(values, key=lambda item: item[1])
        if len(values) == 1:
            best_text = best_name
        else:
            runner_up = min(value for name, value in values if name != best_name)
            ratio = safe_ratio(runner_up, best_value)
            best_text = best_name if ratio is None else f"{best_name} {ratio:.2f}x"
        row += f" {truncate_middle(best_text, 12):>12}"
    else:
        row += f" {'-':>12}"
    print(row)


def print_totals(
    totals: dict[str, float],
    unit: str,
) -> None:
    if not totals:
        return
    print()
    print("Totals (sum of per-file medians):")
    best_total = min(totals.values())
    for name, value in sorted(totals.items(), key=lambda item: item[1]):
        ratio = safe_ratio(value, best_total)
        ratio_text = "inf" if ratio is None else f"{ratio:.2f}x"
        print(
            f"  {name:12} {format_metric(value, unit):>14} {unit:>2}  {ratio_text} vs best"
        )


def write_tsv(
    path: Path,
    results: list[dict[str, Any]],
    compilers: list[CompilerSpec],
    unit: str,
) -> None:
    metric_suffix = unit.lower()
    compiler_names = [compiler.name for compiler in compilers]

    with path.open("w", encoding="utf-8", newline="") as fp:
        writer = csv.writer(fp, delimiter="\t")
        header = ["file", "size_bytes", "status", "best_compiler"]
        for name in compiler_names:
            header.extend(
                [
                    f"{name}_min_{metric_suffix}",
                    f"{name}_median_{metric_suffix}",
                    f"{name}_mean_{metric_suffix}",
                    f"{name}_max_{metric_suffix}",
                    f"{name}_stddev_{metric_suffix}",
                    f"{name}_error",
                ]
            )
        writer.writerow(header)

        for result in results:
            row = [
                result.get("file", ""),
                result.get("size_bytes", ""),
                result.get("status", ""),
                result.get("best_compiler", ""),
            ]
            result_compilers = result.get("compilers", {})
            for name in compiler_names:
                summary = result_compilers.get(name, {})
                for key in ("min", "median", "mean", "max", "stddev"):
                    value = summary.get(key)
                    if isinstance(value, (int, float)):
                        row.append(f"{float(value):.6f}")
                    else:
                        row.append("")
                error = summary.get("error")
                row.append(
                    error.replace("\r", "\\r").replace("\n", "\\n")
                    if isinstance(error, str)
                    else ""
                )
            writer.writerow(row)


def run_benchmark_for_runner(
    *,
    runner: str,
    args: argparse.Namespace,
    compilers: list[CompilerSpec],
    sources: list[Path],
    work_dir: Path,
    hyperfine_extra_args: list[str],
    callgrind_extra_args: list[str],
) -> tuple[dict[str, Any], int]:
    unit = "Ir" if runner == "callgrind" else "ms"
    runner_warmup = None
    runner_repeats = None
    if runner == "hyperfine":
        runner_warmup = args.hyperfine_warmup
        runner_repeats = args.hyperfine_repeats
    elif runner == "internal":
        runner_warmup = args.internal_warmup
        runner_repeats = args.internal_repeats
    print_runner_header(runner, unit, compilers, runner_warmup, runner_repeats)

    results: list[dict[str, Any]] = []
    failed = 0

    for source in sources:
        size_bytes = source.stat().st_size
        safe_source = sanitize_name(str(source))
        file_result: dict[str, Any] = {
            "file": str(source),
            "size_bytes": size_bytes,
            "status": "ok",
            "compilers": {},
        }

        for compiler in compilers:
            out_pyc = work_dir / f"{runner}_{compiler.name}_{safe_source}.pyc"
            if runner == "hyperfine":
                summary, err = benchmark_with_hyperfine(
                    runner_name=runner,
                    compiler=compiler,
                    source=source,
                    out_pyc=out_pyc,
                    warmup=args.hyperfine_warmup,
                    repeats=args.hyperfine_repeats,
                    extra_args=hyperfine_extra_args,
                    work_dir=work_dir,
                )
            elif runner == "callgrind":
                summary, err = benchmark_with_callgrind(
                    runner_name=runner,
                    compiler=compiler,
                    source=source,
                    out_pyc=out_pyc,
                    extra_args=callgrind_extra_args,
                    work_dir=work_dir,
                )
            else:
                for _ in range(args.internal_warmup):
                    _, err = time_one_run(compiler, source, out_pyc)
                    if err is not None:
                        file_result["compilers"][compiler.name] = {"error": err}
                        file_result["status"] = "failed"
                        break
                if file_result["status"] == "failed":
                    break

                samples: list[float] = []
                err = None
                for _ in range(args.internal_repeats):
                    elapsed, err = time_one_run(compiler, source, out_pyc)
                    if err is not None:
                        file_result["compilers"][compiler.name] = {"error": err}
                        file_result["status"] = "failed"
                        break
                    samples.append(elapsed)
                if err is None:
                    summary = summarize_samples(samples, unit="ms")
                else:
                    summary = None

            if err is not None:
                file_result["compilers"][compiler.name] = {"error": err}
                file_result["status"] = "failed"
                break

            assert summary is not None
            file_result["compilers"][compiler.name] = summary

        if file_result["status"] == "ok":
            medians: dict[str, float] = {}
            for name, summary in file_result["compilers"].items():
                value = value_for_summary(summary)
                if value is not None:
                    medians[name] = value
            if medians:
                file_result["best_compiler"] = min(medians, key=medians.get)
            print_file_result_row(
                source, size_bytes, compilers, unit, file_result["compilers"]
            )
        else:
            failed += 1
            print_file_result_row(
                source, size_bytes, compilers, unit, file_result["compilers"]
            )
            if args.fail_fast:
                results.append(file_result)
                break

        results.append(file_result)

    totals: dict[str, float] = {}
    ok_results = [result for result in results if result.get("status") == "ok"]
    for compiler in compilers:
        values = [
            value
            for result in ok_results
            if (value := value_for_summary(result["compilers"].get(compiler.name, {})))
            is not None
        ]
        if values:
            totals[compiler.name] = sum(values)

    print_totals(totals, unit)
    print()

    benchmark_result = {
        "runner": runner,
        "metric_unit": unit,
        "warmup": runner_warmup,
        "repeats": runner_repeats,
        "results": results,
        "totals": totals,
        "failed_files": failed,
    }
    return benchmark_result, failed


def main() -> int:
    args = parse_args()

    try:
        compilers = build_compilers(args)
        runners, hyperfine_path, callgrind_path = resolve_runners(args.runner)
        hyperfine_extra_args = parse_passthrough_args(
            args.hyperfine_arg, "--hyperfine-arg"
        )
        callgrind_extra_args = parse_passthrough_args(
            args.callgrind_arg, "--callgrind-arg"
        )
    except ValueError as err:
        print(f"error: {err}", file=sys.stderr)
        return 2

    if "hyperfine" in runners and (
        args.hyperfine_warmup < 0 or args.hyperfine_repeats <= 0
    ):
        print(
            "error: --hyperfine-warmup must be >= 0 and "
            "--hyperfine-repeats must be > 0",
            file=sys.stderr,
        )
        return 2
    if "internal" in runners and (
        args.internal_warmup < 0 or args.internal_repeats <= 0
    ):
        print(
            "error: --internal-warmup must be >= 0 and --internal-repeats must be > 0",
            file=sys.stderr,
        )
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

    if args.sources:
        sources = [Path(s) for s in args.sources]
    else:
        lib_root = Path(args.lib_root)
        if not lib_root.is_dir():
            print(f"error: --lib-root does not exist: {lib_root}", file=sys.stderr)
            return 2
        sources = find_top_n_largest_py_files(lib_root, args.top_n)

    missing = [source for source in sources if not source.is_file()]
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
    for compiler in compilers:
        print(
            f"  compiler {compiler.name:>12}: {describe_compiler(compiler)} "
            f"({compiler.compiler_kind})"
        )
    print(f"  runners:      {', '.join(runners)}")
    print(f"  affinity:     {affinity}")
    print(f"  files:        {len(sources)}")
    print()

    benchmark_results: list[dict[str, Any]] = []
    failed_any = 0
    for runner in runners:
        benchmark_result, failed = run_benchmark_for_runner(
            runner=runner,
            args=args,
            compilers=compilers,
            sources=sources,
            work_dir=work_dir,
            hyperfine_extra_args=hyperfine_extra_args,
            callgrind_extra_args=callgrind_extra_args,
        )
        benchmark_results.append(benchmark_result)
        failed_any += failed
        if args.fail_fast and failed > 0:
            break

    if args.json_out:
        payload: dict[str, Any] = {
            "config": {
                "compilers": [
                    {
                        "name": compiler.name,
                        "kind": compiler.compiler_kind,
                        "python_cmd": compiler.python_cmd or [],
                        "compiler_path": (
                            str(compiler.compiler_path)
                            if compiler.compiler_path is not None
                            else ""
                        ),
                    }
                    for compiler in compilers
                ],
                "requested_runners": args.runner or ["auto"],
                "resolved_runners": runners,
                "runner_settings": {
                    "hyperfine": {
                        "warmup": args.hyperfine_warmup,
                        "repeats": args.hyperfine_repeats,
                        "extra_args": hyperfine_extra_args,
                    },
                    "internal": {
                        "warmup": args.internal_warmup,
                        "repeats": args.internal_repeats,
                    },
                    "callgrind": {
                        "extra_args": callgrind_extra_args,
                    },
                },
                "work_dir": str(work_dir),
                "cpu_affinity": affinity,
                "cpu_affinity_request": args.cpu_affinity,
                "hyperfine_path": hyperfine_path or "",
                "callgrind_path": callgrind_path or "",
            },
            "benchmarks": benchmark_results,
        }
        Path(args.json_out).write_text(json.dumps(payload, indent=2) + "\n")
        print(f"Wrote JSON results to: {args.json_out}")

    if args.tsv_out:
        for benchmark_result in benchmark_results:
            tsv_path = output_path_for_runner(
                args.tsv_out,
                benchmark_result["runner"],
                len(benchmark_results),
            )
            write_tsv(
                tsv_path,
                benchmark_result["results"],
                compilers,
                benchmark_result["metric_unit"],
            )
            print(f"Wrote TSV results to: {tsv_path}")

    if failed_any > 0:
        print(f"Completed with {failed_any} failing file(s).", file=sys.stderr)
        return 1

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
