b = {"x": 44}

def foo():
    return {**b}
print(foo())

def bar():
    return {"jo":42, **dd}

dd = {"ll": 4}
print(bar())
dd = {"jo": 4, False: ...}
print(bar())

def baz():
    return {**dd, "jo": 87}

dd = {"ll": 4}
print(bar())
dd = {"jo": 4, False: ...}
print(bar())

def bam():
    return {**dd, **dd2, "x": 42, ...:True, **dd3, "ll": None}
dd2 = {}
dd = {4: 5}
dd3 = {True: False, "ll": 44}
print(bam())
