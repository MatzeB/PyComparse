def f(x):
    print("f(" + str(x) + ")")
    return x

def test(x, y):
    if f(x):
        print("then 1")
    elif f(y) == "number 2":
        print("then 2")
    elif f(y):
        print("then 3")
    else:
        print("else")
    print("footer")

test(0, False)
test(False, "!")
test("?", "")
test("!", "?")
test("", "number 2")

def test2(x, y):
    if f(x):
        print("then 1")
    elif f(y):
        print("then 2")
    print("footer")

test2(0, False)
test2(False, "!")
test2("?", "")
test2("!", "?")
