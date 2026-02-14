class A:
    def f(self):
        return "A"


class X(A):
    def f(self):
        return super().f()

    __class__ = 413
    y = __class__


print(X().f())
print(X.__dict__["__class__"])
print(X.__dict__["y"])
