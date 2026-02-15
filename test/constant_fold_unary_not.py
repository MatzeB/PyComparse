def not_int_one():
    return not 1


def not_int_zero():
    return not 0


def not_empty_string():
    return not ""


def not_nonempty_tuple():
    return not (1, 2)


def not_none():
    return not None


print(not_int_one.__code__.co_consts)
print(not_int_zero.__code__.co_consts)
print(not_empty_string.__code__.co_consts)
print(not_nonempty_tuple.__code__.co_consts)
print(not_none.__code__.co_consts)
print(
    not_int_one(),
    not_int_zero(),
    not_empty_string(),
    not_nonempty_tuple(),
    not_none(),
)
