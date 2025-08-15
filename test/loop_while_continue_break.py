def f(x):
    print("f(" + str(x) + ")")
    return x

a = True
b = 0
while f(a) and f(b < 15):
    b += 1
    if b == 5:
        continue
    a = not a
    if b == 12:
        break
    print(".")
