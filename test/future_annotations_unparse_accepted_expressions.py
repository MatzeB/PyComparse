from __future__ import annotations

a00: x
a01: None
a02: True
a03: False
a04: ...
a05: 0
a06: 123456789012345678901234567890
a07: 1.25
a08: 1e100
a09: 1j
a10: (1 + 2j)
a11: "s"
a12: b"bytes"
a13: obj.attr
a14: arr[0]
a15: arr[1:2]
a16: arr[1:2:3]
a17: f()
a18: f(1, *args, **kwargs)
a19: (a,)
a20: (a, b)
a21: [a, b]
a22: {a, b}
a23: {a: b}
a24: {a: b, **d}
a25: (x for x in y)
a26: [x for x in y]
a27: {x for x in y}
a28: {x: y for (x, y) in z}
a29: f"{x}"
a30: not x
a31: ~x
a32: -x
a33: +x
a34: x or y
a35: x and y
a36: x < y < z
a37: x == y
a38: x + y
a39: x - y
a40: x * y
a41: x @ y
a42: x / y
a43: x // y
a44: x % y
a45: x ** y
a46: x << y
a47: x >> y
a48: x & y
a49: x ^ y
a50: x | y
a51: x if c else y
a52: lambda a, b=1: a + b
a53: (x := 1)
a54: await x
a55: (1 + 2j) + 3
a56: 3 + (1 + 2j)
a57: (1 + 2j) - (3 + 4j)
a58: (1 + 2j) * 2
a59: (1 + 2j) / 2
a60: (1 + 2j) ** 2
a61: -(1 + 2j)
a62: +(1 + 2j)
a63: 3 in {1, 2, 3}
a64: 3 not in {1, 2, 3}
a65: (1, 2) + (3, 4)
a66: (1,) * 3

for i in range(67):
    key = f"a{i:02d}"
    print(__annotations__[key])
