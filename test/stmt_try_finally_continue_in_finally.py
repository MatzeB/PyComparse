def continue_in_finally_basic():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
        finally:
            out.append("f" + str(i))
            if i < 2:
                continue
        out.append("x" + str(i))
    return out


def continue_in_finally_overrides_return():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
            if i == 0:
                return "TRY", out
        finally:
            out.append("f" + str(i))
            if i == 0:
                continue
        out.append("x" + str(i))
    return "AFTER", out


print(continue_in_finally_basic())
print(continue_in_finally_overrides_return())
