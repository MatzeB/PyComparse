def f():
    if False:
        def inner():
            return 1
    return 2


print(f())
