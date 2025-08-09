class C:
    @property
    def foo(self):
        return 42

print(C().foo)

def myattr(x, y):
    print("myattr %s, %s" % (x, y))
    def foo(f):
        print("inner")
        return f
    return foo

def replace_with_abs(f):
    print("replace_with_abs")
    return abs

@myattr(3, 4)

@replace_with_abs
def foobar(x):
    return x + 7

print(foobar(-3))

