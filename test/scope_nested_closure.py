x = 10

def outer():
    def inner():
        return x

    x = 20
    return inner

print(outer()())


def chain():
    value = 1

    def middle():
        def leaf():
            return value

        return leaf

    return middle()()

print(chain())
