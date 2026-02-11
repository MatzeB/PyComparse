"module doc"

"ignored module string"


def f_doc():
    "function doc"
    return 1


def f_nodoc():
    return 1


def f_bytes():
    b"bytes are not a docstring"
    return 1


def f_doc_implicit():
    "implicit doc"
    x = 1


def f_two_strings():
    "first doc"
    "second string"
    return 1


class C_doc:
    "class doc"
    x = 1


class C_nodoc:
    x = 1
    "ignored class string"


print(__doc__)
print(f_doc.__doc__)
print(f_nodoc.__doc__)
print(f_bytes.__doc__)
print(f_doc_implicit.__doc__)
print(f_two_strings.__doc__)
print(C_doc.__doc__)
print(C_nodoc.__doc__)

print(f_doc.__code__.co_consts[0])
print(f_nodoc.__code__.co_consts[0] is None)
print(f_doc_implicit.__code__.co_consts[0])
