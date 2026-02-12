"""module doc"""

from __future__ import annotations


def side():
    print("side")
    return int


x: side()
print(__annotations__)
