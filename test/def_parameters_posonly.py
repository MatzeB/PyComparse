def foo(x, /):
    print(x)
foo(4)

def foo(x, /, **kwargs):
    print(x)
    print(kwargs)
foo(4)
foo(2, x=4)
