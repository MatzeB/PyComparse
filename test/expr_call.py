def foo(a, b, c, d):
    print(a, b, c, d)

a = (5, 99, 6, -8)
foo(*a)

c = (55, -13, 11)
foo(42, *c)
foo(*c, 42)

a = 7
b = "hello"
t = (-9,)
y = (88,)
foo(*t, a, b, *y)
