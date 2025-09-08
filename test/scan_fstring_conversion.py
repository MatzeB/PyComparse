def foo(x):
    print(f"none: {x}")
    print(f"repr: {x!r}")
    print(f"str: {x!s}")
    print(f"ascii: {x!a}")

foo(0.)
foo("hello")
foo("ta\tb")
foo(...)
