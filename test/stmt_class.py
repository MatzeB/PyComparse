class C:
    def __init__(self, x):
        self.x = x

c = C(42)
print(c.x)
print(type(c))

class Empty:
    pass

class B(list, Empty):
    pass

b = B(range(5))
print(b)
print(type(b))
print(isinstance(b, Empty))
