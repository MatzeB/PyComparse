from threading import Thread

def foo():
    print("foo called")

assert True, foo()

def t_assert(x):
    assert x, foo()
    print("end of t_assert")

t_assert("!")

# TODO: enable when try/except works
#try:
#    t_assert("")
#except AssertionError as e:
#    print(repr(e))
