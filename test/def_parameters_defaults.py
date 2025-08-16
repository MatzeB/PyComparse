def bar(a, b, c=42, d=("jo", False)):
    print(a, b, c, d)
bar(1, 2)
bar(1, 2, 3)
bar(1, 2, d="!")

def foo(a, b="xx", c=..., *args, ka=5, kb, kc=False, kd=None, **kwargs):
    print(a, b, c)
    print(args)
    print(ka, kb, kc, kd)
    print(kwargs)
foo(2, kb=5)
foo(2, kb=5, ka=11)
foo(1, 2, 3, 4, 5, kd=66, kb="!")
foo(1, kd=66, kb="!", jojo="boy")
