def outer():
    x = 1

    def inner():
        nonlocal x
        x += 1
        return x

    inner()
    return inner()

print(outer())
