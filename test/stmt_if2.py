def f(x):
    print("f(" + str(x) + ")")
    return x

def test(a, b, c):
    print("----")
    if f(a):
        print("t1")
    if not f(a):
        print("t2")
    if f(a) and f(b):
        print("t3")
    if f(a) or f(b):
        print("t4")
    if not (f(a) and f(b)):
        print("t5")
    if not (f(a) or f(b)):
        print("t6")
    if not (f(a) or f(b) and f(c)):
        print("t7")

test(None, 0, False)
test(0, 0, 1)
test(0, "jo", 0)
test(0, "jo", True)
test(42, False, None)
test("X", "", "!")
test(True, 44, None)
test(1, 2, 3)
