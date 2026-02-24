def break_in_finally_basic():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
        finally:
            out.append("f" + str(i))
            break
        out.append("x" + str(i))
    return out


def break_in_finally_overrides_return():
    out = []
    for i in range(2):
        try:
            out.append("t" + str(i))
            return "TRY", out
        finally:
            out.append("f" + str(i))
            break
    return "AFTER", out


print(break_in_finally_basic())
print(break_in_finally_overrides_return())
