l = lambda: 42
print(l())
print(l.__name__)

l2, y = lambda x, y=4: x * y, "jo"
print(y)
print(l2(2))
print(l2(-2, 7))

l3 = lambda x, *args, foo, bar=42, end: (x, args, foo, bar, end)
print(l3(4, 5, 6, foo=True, end=...))
