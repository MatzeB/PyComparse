def f0():
    print("f0")
f0()

def f1(x):
    print(x)
f1(42)

def f1_(x,):
    print("f1_", x)
f1_(13)

def f2(x, y):
    print(x, y)
f2("hello", "world")

def f2_(x, y,):
    print("f2_", x, y)
f2_("greet", "user")

def f0(*a):
    print(a)
f0()
f0(11)
f0(True, None, ...)
f0(*range(4))

def f1(**kwargs):
    print(kwargs)
f1()
f1(x=42)
f1(x="x", y="y")
# f1(**{"jo": 42, "man": 69})

def f1_(**kwargs,):
    print("f1_", kwargs)
f1_()
f1_(x=42)
f1_(x="x", y="y")
#f1_(**{"jo": 42, "man": 69})
