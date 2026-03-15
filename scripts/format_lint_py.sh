#!/bin/sh
set -eu

uv run ruff format scripts
uv run ruff check scripts
