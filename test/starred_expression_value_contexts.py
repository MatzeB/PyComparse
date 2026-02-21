from __future__ import annotations

a = [4, 5, 6]
x = 1, *a
y = (*a,)


def f():
    z = [2, 3]
    return 1, *z


def g():
    rest = 4, 5, 6
    yield 1, 2, 3, *rest


def ann(rest):
    v: int = 1, *rest
    return v


print(x)
print(y)
print(f())
print(list(g()))
print(ann((7, 8)))
