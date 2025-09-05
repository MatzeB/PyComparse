x = {str(x):x+3 for x in range(5)}
print(x)
print(type(x))

y = {x:x*x for x in range(20) if x not in (3, 8, 13)}
print(y)

z = {str(y-1):x+y for x in range(20) if x not in (3, 8, 13) for y in range(3)}
print(z)
