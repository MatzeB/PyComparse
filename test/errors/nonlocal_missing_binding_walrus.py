def outer():
    def inner():
        nonlocal x
        [x := i for i in range(3)]

    inner()
