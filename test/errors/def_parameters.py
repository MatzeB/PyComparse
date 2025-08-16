def f0(a, b=2, c):
    pass

def f1(a, *b, *c):
    pass

def f2(a, *, *c):
    pass

def f3(a, *a):
    pass

def f4(*):
    pass

def f5(*, **x):
    pass

def f6(**kwargs, a):
    pass

def f7(**kwargs, *b):
    pass

def f8(**kwargs, **b):
    pass

def f9(**kwargs, /);
    pass

def f10(a, *args, /):
    pass
