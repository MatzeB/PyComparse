print([*range(3)])
print([True, *range(2)])
l = []
print([False, *l])
l2 = [..., None]
print([1, 2, *range(2), *l2, 1, *range(5,1,-2)])
print([1, 2, *range(2), *l2, 1, *range(5,1,-2), 88])
print([1, 2, *range(2), *l2, 1, *range(5,1,-2), 88, 98])
