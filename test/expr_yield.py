def foo():
    print((yield 42))
    print((yield "hello"))
    yield None

for x in foo():
    print(x)

g = foo()
next(g)
print(g.send("send0"))
print(g.send("SeNt1"))

def noarg_yield():
    print((yield))
    yield 44

for x in noarg_yield():
    print(x)

def unreachable_yield():
    return
    print((yield ...))

for y in unreachable_yield():
    print(y)
