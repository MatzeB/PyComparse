def fold_string_add():
    return "a" + "b"


def fold_bytes_add():
    return b"a" + b"b"


def fold_tuple_add():
    return (1, 2) + (3, 4)


def fold_string_repeat():
    return "ab" * 3


def fold_string_repeat_reverse():
    return 3 * "ab"


def fold_tuple_repeat():
    return (1, 2) * 3


def fold_tuple_repeat_reverse():
    return 3 * (1, 2)


def not_fold_string_negative_repeat():
    return "a" * -1


def not_fold_tuple_negative_repeat():
    return (1, 2) * -1


print(fold_string_add.__code__.co_consts)
print(fold_bytes_add.__code__.co_consts)
print(fold_tuple_add.__code__.co_consts)
print(fold_string_repeat.__code__.co_consts)
print(fold_string_repeat_reverse.__code__.co_consts)
print(fold_tuple_repeat.__code__.co_consts)
print(fold_tuple_repeat_reverse.__code__.co_consts)
print(not_fold_string_negative_repeat.__code__.co_consts)
print(not_fold_tuple_negative_repeat.__code__.co_consts)
print(
    fold_string_add(),
    fold_bytes_add(),
    fold_tuple_add(),
    fold_string_repeat(),
    fold_string_repeat_reverse(),
    fold_tuple_repeat(),
    fold_tuple_repeat_reverse(),
    not_fold_string_negative_repeat(),
    not_fold_tuple_negative_repeat(),
)
