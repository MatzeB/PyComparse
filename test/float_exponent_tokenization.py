assert (1 if 1else 0) == 1
assert (1 if 0else 0) == 0

try:
    eval("0 if 1Else 0")
except SyntaxError:
    pass
else:
    raise AssertionError("expected SyntaxError")
