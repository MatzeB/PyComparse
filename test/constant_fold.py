def folded_add():
    return 1 + 2


def folded_sub():
    return 9 - 4


def folded_tuple():
    return (1 + 2, 9 - 4, (6 - 1, 2 + 3))


def folded_sub_negative():
    return 1 - 2


def folded_sub_negative_long():
    return 1 - 4611686018427387905


def folded_unary_negative():
    return -(2 + 3)


def folded_mul():
    return -7 * 6


def folded_floor_div():
    return -7 // 3


def folded_mod():
    return -7 % 3


def folded_bitops():
    return (6 & 3, 6 | 3, 6 ^ 3)


def folded_shifts():
    return (3 << 4, 17 >> 2, -17 >> 2)


def folded_invert():
    return ~5


def folded_power():
    return 3**5


def not_folded_negative_shift():
    return 1 << -1


def not_folded_int64_min():
    return -4611686018427387904 - 4611686018427387904


print(folded_add.__code__.co_consts)
print(folded_sub.__code__.co_consts)
print(folded_tuple.__code__.co_consts)
print(folded_sub_negative.__code__.co_consts)
print(folded_sub_negative_long.__code__.co_consts)
print(folded_unary_negative.__code__.co_consts)
print(folded_mul.__code__.co_consts)
print(folded_floor_div.__code__.co_consts)
print(folded_mod.__code__.co_consts)
print(folded_bitops.__code__.co_consts)
print(folded_shifts.__code__.co_consts)
print(folded_invert.__code__.co_consts)
print(folded_power.__code__.co_consts)
print(not_folded_negative_shift.__code__.co_consts)
print(
    folded_add(),
    folded_sub(),
    folded_tuple(),
    folded_sub_negative(),
    folded_sub_negative_long(),
    folded_unary_negative(),
    folded_mul(),
    folded_floor_div(),
    folded_mod(),
    folded_bitops(),
    folded_shifts(),
    folded_invert(),
    folded_power(),
    not_folded_int64_min(),
)
