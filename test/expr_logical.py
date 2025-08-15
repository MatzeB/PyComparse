def f(x):
    print("f(" + str(x) + ")")
    return x

print("" or f(None))
print(False or f(42))
print(... or f(None))
print(4 or f("jo"))

print("" and f(None))
print(False and f(42))
print(... and f(None))
print(4 and f("jo"))

print(not 42)
print(not False)
print(not "")
