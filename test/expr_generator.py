print(tuple(x for x in range(5)))

print(tuple(x for x in range(20) if x not in (3, 8, 13)))

print(tuple(x+y for x in range(20) if x not in (3, 8, 13) for y in range(3)))

x = {"foo": 42, "bar": 8}
print(tuple(str(value) + " -> " + key for key, value in x.items()))

print(tuple(x for x in range(33) if x % 2 == 0 and x % 3 == 0))
