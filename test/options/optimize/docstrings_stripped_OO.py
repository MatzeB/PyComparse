"""Module docstring"""


def foo():
    """Function docstring"""
    pass


class Bar:
    """Class docstring"""
    pass


print(repr(__doc__))
print(repr(foo.__doc__))
print(repr(Bar.__doc__))
