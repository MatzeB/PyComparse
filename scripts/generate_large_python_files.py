#!/usr/bin/env python3
"""Generate large synthetic Python source files for parser/compile benchmarks."""

from __future__ import annotations

import argparse
from pathlib import Path


STATEMENT_BLOCK = (
    "x = 1\n"
    "y = x + 2\n"
    "z = y * 3\n"
    "if z % 2 == 0:\n"
    "    x = z // 2\n"
    "else:\n"
    "    x = z + 1\n"
    "\n"
)

FUNCTION_BLOCK_TEMPLATE = (
    "def generated_func_{idx}():\n"
    "    a = 1\n"
    "    b = a + 2\n"
    "    c = b * 3\n"
    "    if c % 2 == 0:\n"
    "        return c // 2\n"
    "    return c + 1\n"
    "\n"
)


def parse_sizes_mb(raw: str) -> list[int]:
    parts = [p.strip() for p in raw.split(",") if p.strip()]
    if not parts:
        raise ValueError("empty --sizes-mb")
    sizes = [int(p) for p in parts]
    if any(size <= 0 for size in sizes):
        raise ValueError("all values in --sizes-mb must be > 0")
    return sizes


def build_path(output_dir: Path, size_mb: int, pattern: str, index: int) -> Path:
    if index == 0:
        suffix = ""
    else:
        suffix = f"_{index}"
    return output_dir / f"huge_{pattern}_{size_mb}mb{suffix}.py"


def generate_one_file(path: Path, target_size_mb: int, pattern: str) -> int:
    target_bytes = target_size_mb * 1024 * 1024
    path.parent.mkdir(parents=True, exist_ok=True)

    header = (
        "# Auto-generated benchmark file\n"
        f"# target_size_mb={target_size_mb}\n"
        f"# pattern={pattern}\n\n"
    )

    with path.open("w", encoding="utf-8", newline="\n") as fp:
        fp.write(header)

        if pattern == "statements":
            block = STATEMENT_BLOCK
            while fp.tell() < target_bytes:
                fp.write(block)
        else:
            idx = 0
            while fp.tell() < target_bytes:
                fp.write(FUNCTION_BLOCK_TEMPLATE.format(idx=idx))
                idx += 1

        fp.write("\n")
        size_bytes = fp.tell()

    return size_bytes


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Generate very large Python files for stress/performance testing. "
            "Example: --sizes-mb 100,200"
        )
    )
    parser.add_argument(
        "--output-dir",
        default="test/perf_generated",
        help="Directory to place generated files.",
    )
    parser.add_argument(
        "--sizes-mb",
        default="100,200",
        help="Comma-separated list of file sizes in MB (default: 100,200).",
    )
    parser.add_argument(
        "--pattern",
        choices=("statements", "functions"),
        default="statements",
        help="Code pattern used to fill files.",
    )
    parser.add_argument(
        "--copies",
        type=int,
        default=1,
        help="Generate this many files per requested size (default: 1).",
    )
    args = parser.parse_args()

    if args.copies <= 0:
        raise SystemExit("error: --copies must be > 0")

    try:
        sizes_mb = parse_sizes_mb(args.sizes_mb)
    except ValueError as err:
        raise SystemExit(f"error: {err}") from err

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    print(f"Generating files in {output_dir} ...")
    for size_mb in sizes_mb:
        for i in range(args.copies):
            path = build_path(output_dir, size_mb, args.pattern, i)
            size_bytes = generate_one_file(path, size_mb, args.pattern)
            print(f"  wrote {path} ({size_bytes} bytes)")

    print("Done.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
