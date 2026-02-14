x = 1

# Long debug field with a long run of spaces before '=' to exercise
# scan_skip_inline_spaces while capture is active.
print(
    f"{x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x                                                            =}"
)

# Nested debug fields with a long inner expression to exercise capture depth
# and refill handling.
print(
    f"{f'{x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x                                                            =}'=}"
)

# Multiline debug field to make sure newlines and indentation survive capture
# across multiple refills.
print(
    f"""{(
x
+ x + x + x + x + x + x + x + x + x + x
+ x + x + x + x + x + x + x + x + x + x
+ x + x + x + x + x + x + x + x + x + x
+ x + x + x + x + x + x + x + x + x + x
)=}"""
)

# Refill cases that combine debug '=', conversion and format spec.
print(
    f"{x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x                                                            =!r:>8}"
)
print(
    f"{f'{x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x                                                            =!r:>8}'=!r:>70}"
)
