x = {x for x in range(5)}
print(x)
print(type(x))

y = {x for x in range(20) if x not in (3, 8, 13)}
print(y)

z = {x+y for x in range(20) if x not in (3, 8, 13) for y in range(3)}
print(z)
