class X:
    def f(self, __a=41):
        return __a + 1

    def g(self, *, __b=42):
        return __b


x = X()
print(x.f())
print(x.g())
