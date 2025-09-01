x, *rest = (1, 2, 3, 4)
print(x)
print(rest)

x, (y, *rest), z = (1, (3, 4, 5), 6)
print(x)
print(y)
print(rest)
