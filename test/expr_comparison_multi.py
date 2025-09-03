def f(x):
    print("f(" + str(x) + ")")
    return x

def compare_condjump_le(a, b, c):
    if f(a) <= f(b) <= f(c):
        print("true")
    else:
        print("false")
compare_condjump_le(1, 5, 10)
compare_condjump_le(1, 0, 10)
compare_condjump_le(1, 11, 10)

def compare_condjump_foo(a, b, c, d):
    if not (f(a) not in f(b) < f(c) is f(d)):
        print("false")
    print("end")

t = (2, 0, 0)
compare_condjump_foo(1, (1, 2, 3), t, t)
t2 = (1, 2, 3)
compare_condjump_foo(33, (1, 2, 3), t2, t)
compare_condjump_foo(33, (1, 2, 3), t, t2)
compare_condjump_foo(33, (1, 2, 3), t, t)

def compare_value_le(a, b, c):
    print(f(a) <= f(b) <= f(c))

compare_value_le(1, 5, 10)
compare_value_le(1, 0, 10)
compare_value_le(1, 11, 10)

def compare_value_foo(a, b, c, d):
    print(f(a) not in f(b) < f(c) is f(d))

t = (2, 0, 0)
compare_value_foo(1, (1, 2, 3), t, t)
t2 = (1, 2, 3)
compare_value_foo(33, (1, 2, 3), t2, t)
compare_value_foo(33, (1, 2, 3), t, t2)
compare_value_foo(33, (1, 2, 3), t, t)
