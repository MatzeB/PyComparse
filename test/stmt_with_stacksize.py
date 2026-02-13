from contextlib import nullcontext


def one():
    with nullcontext():
        pass


def two():
    with nullcontext():
        with nullcontext():
            pass


print(one.__code__.co_stacksize)
print(two.__code__.co_stacksize)
