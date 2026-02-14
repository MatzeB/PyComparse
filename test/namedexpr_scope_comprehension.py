def scope_local():
    res = [j := i for i in range(5)]
    return res, j


GLOBAL_VAR = None


def scope_global():
    global GLOBAL_VAR
    sentinel = 99
    [GLOBAL_VAR := sentinel for _ in range(1)]
    return GLOBAL_VAR


def scope_nonlocal():
    x = 0

    def inner():
        nonlocal x
        [x := i for i in range(3)]

    inner()
    return x


print(scope_local())
print(scope_global())
print(scope_nonlocal())
