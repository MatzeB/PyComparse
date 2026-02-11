class C:
    def f(self):
        nonlocal __class__
        return __class__
