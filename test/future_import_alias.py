from __future__ import annotations as ann


def side():
    print("side")
    return int


print(type(ann).__name__)
x: side()
print(__annotations__)
