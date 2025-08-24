in_global: int = 42

class C:
    print(__annotations__)

class D:
    in_class: str = ""
    print(__annotations__)

class X:
    print(__annotations__)
    if False:
        a: int
