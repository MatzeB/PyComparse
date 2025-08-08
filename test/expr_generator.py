print(tuple(x for x in range(5)))

print(tuple(x for x in range(20) if x not in (3, 8, 13)))

print(tuple(x+y for x in range(20) if x not in (3, 8, 13) for y in range(3)))
