def break_case():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
            if i == 1:
                break
        finally:
            out.append("f" + str(i))
    return out


def continue_case():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
            if i == 1:
                continue
        finally:
            out.append("f" + str(i))
        out.append("x" + str(i))
    return out


def return_case():
    out = []

    def g():
        try:
            out.append("t")
            return "R"
        finally:
            out.append("f")

    return g(), out


def except_return_case():
    out = []

    def g():
        try:
            1 / 0
        except ZeroDivisionError:
            out.append("e")
            return "ER"
        finally:
            out.append("f")

    return g(), out


print(break_case())
print(continue_case())
print(return_case())
print(except_return_case())
