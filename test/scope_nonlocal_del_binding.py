def outer():
    def inner():
        nonlocal frame
        frame = "ok"

    try:
        del frame
    except UnboundLocalError:
        pass

    inner()
    return frame


print(outer())
