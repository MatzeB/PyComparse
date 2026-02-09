x = 1


def outer():
    x = 5

    def inner():
        global x
        x += 10

    inner()
    return x


print(outer())
print(x)
