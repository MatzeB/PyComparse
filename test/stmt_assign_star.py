x, *rest = (1, 2, 3, 4)
print(x)
print(rest)

*prefix, y = (1, 2, 3, 4)
print(y)
print(prefix)

a, *middle, b = (1, 2, 3, 4)
print(a)
print(middle)
print(b)

u, (v, *rest), w = (1, (3, 4, 5), 6)
print(u)
print(v)
print(rest)
print(w)
