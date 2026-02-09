x = 10


def f():
    try:
        print(x)
    except UnboundLocalError:
        print('unbound')
    x = 5


f()
