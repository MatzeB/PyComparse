def return_in_finally_override():
    def f():
        try:
            return "TRY"
        finally:
            return "FINALLY"

    return f()


def nested_except_finally_return_override():
    out = []

    def f():
        try:
            1 / 0
        except ZeroDivisionError:
            out.append("except")
            try:
                return "INNER_TRY"
            finally:
                out.append("inner-finally")
                return "INNER_FINALLY"
        finally:
            out.append("outer-finally")

    return f(), out


def nested_except_finally_return_preserve():
    out = []

    def f():
        try:
            1 / 0
        except ZeroDivisionError:
            out.append("except")
            try:
                return "INNER_TRY"
            finally:
                out.append("inner-finally")
        finally:
            out.append("outer-finally")

    return f(), out


def nested_raise_swallowed_by_finally_return():
    out = []

    def f():
        try:
            raise RuntimeError("r")
        except RuntimeError:
            out.append("except")
            try:
                raise KeyError("k")
            finally:
                out.append("inner-finally")
                return "RET_INNER_FINALLY"
        finally:
            out.append("outer-finally")

    return f(), out


print(return_in_finally_override())
print(nested_except_finally_return_override())
print(nested_except_finally_return_preserve())
print(nested_raise_swallowed_by_finally_return())
