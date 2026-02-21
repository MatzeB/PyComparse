from __future__ import annotations

x: (y := 1)
z: 1j

print(__annotations__["x"])
print(__annotations__["z"])
