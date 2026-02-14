d1 = {"a": 1, "b": 2}
d2 = {"a": (lambda: 42)(), "b": 3}
print(d1["a"], d1["b"])
print(d2["a"], d2["b"])
