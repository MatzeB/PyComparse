print(3 in {1, 2, 3})
print(4 in {1, 2, 3})
print(3 not in {1, 2, 3})
print("a" in {"a", "b"})
print(0 in {0})

def f(x):
    return x in {1, 2, 3}

assert any(isinstance(c, frozenset) for c in f.__code__.co_consts)
assert f(2)
assert not f(5)
print("frozenset folding ok")
