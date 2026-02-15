def build_values(n):
    singletons = [2 ** i for i in range(n)]
    return {x: frozenset([x ^ s for s in singletons]) for x in range(2 ** n)}


values = build_values(4)
print(sorted(values[0]))
print(sorted(values[6]))
