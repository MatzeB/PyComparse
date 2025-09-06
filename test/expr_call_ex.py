def foo(*args, **kwargs):
    print(args)
    print(kwargs)

def ll():
    return {"ghost": "uuuuh", "": 42}

def f0(**kwargs):
    foo(**kwargs)

def f1(**kwargs):
    foo(jojo=42, **kwargs)

def f2(**kwargs):
    foo(jojo=42, bar=8, **kwargs)

def f3(**kwargs):
    foo(jojo=42, **ll(), bar=sum(range(4)), baz=None, **kwargs)

def x(func):
    func()
    func(hello="world")
    func(x=42, dots=...)

x(f0)
x(f1)
x(f2)
x(f3)

l = [1,2,3]
foo(named="jo dawg", *l)
