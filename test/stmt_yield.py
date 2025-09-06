def foo():
    yield 42
    yield "hello"

for x in foo():
    print(x)

def bar():
    yield "->"
    yield from range(3)
    yield ...

for y in bar():
    print(y)

def unreachable_yield():
    return
    yield ...

for y in unreachable_yield():
    print(y)

def noarg_yield():
    yield

for y in noarg_yield():
    print(y)
