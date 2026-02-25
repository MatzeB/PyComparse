f = lambda: (yield 1)
g = f()
print(next(g))
try:
    next(g)
except StopIteration:
    print("done")
