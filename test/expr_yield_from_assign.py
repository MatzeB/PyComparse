def sub():
    yield 1
    return 2


def sub2():
    yield 3
    return 4


def gen():
    x = yield from sub()
    y: int = yield from sub2()
    return x + y


g = gen()
print(next(g))
print(next(g))
try:
    next(g)
except StopIteration as e:
    print(e.value)
