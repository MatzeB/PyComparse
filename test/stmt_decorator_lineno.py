import sys


def dec1(f):
    return f


def make_dec():
    def dec2(f):
        return f

    return dec2


def traced_lines(fn):
    lines = []

    def tracer(frame, event, arg):
        if event == "line" and frame.f_code is fn.__code__:
            lines.append(frame.f_lineno - fn.__code__.co_firstlineno)
        return tracer

    old = sys.gettrace()
    sys.settrace(tracer)
    try:
        fn()
    finally:
        sys.settrace(old)
    return lines


def target():
    @dec1
    @make_dec()
    def inner():
        return 1

    return inner()


print(traced_lines(target))
