def f():
    return __debug__


class C:
    x = __debug__


print(__debug__)
print(f())
print(C.x)
