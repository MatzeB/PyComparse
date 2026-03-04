# Test that "not (a OP b)" is NOT folded into "a NEG_OP b" for rich-comparison
# operators. __eq__ and __ne__ (and the ordered pairs) are separate dunder
# methods and can have independent behaviour, so the fold would be wrong.


class Weird:
    """Returns True from every rich-comparison method."""

    def __eq__(self, other):
        return True

    def __ne__(self, other):
        return True

    def __lt__(self, other):
        return True

    def __le__(self, other):
        return True

    def __gt__(self, other):
        return True

    def __ge__(self, other):
        return True


a, b = Weird(), Weird()

# Each of these must call __eq__/__ne__/... and then negate the result,
# NOT dispatch directly to the logically-opposite method.
print(not (a == b))  # not __eq__()  → not True  → False
print(not (a != b))  # not __ne__()  → not True  → False
print(not (a < b))   # not __lt__()  → not True  → False
print(not (a <= b))  # not __le__()  → not True  → False
print(not (a > b))   # not __gt__()  → not True  → False
print(not (a >= b))  # not __ge__()  → not True  → False

# in/not-in and is/is-not folds ARE correct (same underlying operation).
xs = [1, 2, 3]
print(not (1 in xs))      # False
print(not (4 not in xs))  # False
x = object()
print(not (x is x))       # False
print(not (x is not x))   # False
