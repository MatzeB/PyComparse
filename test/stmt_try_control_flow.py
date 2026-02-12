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


def break_in_try_except():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
            if i == 1:
                break
        except ValueError:
            out.append("e" + str(i))
    return out


def continue_in_try_except():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
            if i == 1:
                continue
        except ValueError:
            out.append("e" + str(i))
        out.append("x" + str(i))
    return out


def break_in_except_handler():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
            1 / 0
        except ZeroDivisionError:
            out.append("e" + str(i))
            if i == 1:
                break
    return out


def continue_in_except_handler():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
            1 / 0
        except ZeroDivisionError:
            out.append("e" + str(i))
            if i == 1:
                continue
        out.append("x" + str(i))
    return out


def break_in_except_as_handler():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
            1 / 0
        except ZeroDivisionError as e:
            out.append("e" + str(i))
            if i == 1:
                break
    return out


def continue_in_except_as_handler():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
            1 / 0
        except ZeroDivisionError as e:
            out.append("e" + str(i))
            if i == 1:
                continue
        out.append("x" + str(i))
    return out


def break_in_except_handler_with_finally():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
            1 / 0
        except ZeroDivisionError:
            out.append("e" + str(i))
            if i == 1:
                break
        finally:
            out.append("f" + str(i))
    return out


def continue_in_except_handler_with_finally():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
            1 / 0
        except ZeroDivisionError:
            out.append("e" + str(i))
            if i == 1:
                continue
        finally:
            out.append("f" + str(i))
        out.append("x" + str(i))
    return out


def break_in_except_as_with_finally():
    out = []
    for i in range(3):
        try:
            out.append("t" + str(i))
            1 / 0
        except ZeroDivisionError as e:
            out.append("e" + str(i))
            if i == 1:
                break
        finally:
            out.append("f" + str(i))
    return out


def nested_try_break():
    out = []
    for i in range(3):
        try:
            try:
                out.append("t" + str(i))
                if i == 1:
                    break
            except ValueError:
                out.append("inner_e")
        except RuntimeError:
            out.append("outer_e")
        finally:
            out.append("f" + str(i))
    return out


def return_in_try_except():
    out = []

    def g():
        try:
            out.append("t")
            return "R"
        except ValueError:
            out.append("e")

    return g(), out


def return_in_except_handler():
    out = []

    def g():
        try:
            1 / 0
        except ZeroDivisionError:
            out.append("e")
            return "ER"

    return g(), out


def return_in_except_as_handler():
    out = []

    def g():
        try:
            1 / 0
        except ZeroDivisionError as e:
            out.append("e")
            return "ER"

    return g(), out


def break_try_loop_try_try():
    out = []
    try:
        for i in range(3):
            try:
                try:
                    out.append("t" + str(i))
                    if i == 1:
                        break
                finally:
                    out.append("inner_f" + str(i))
            except ValueError:
                out.append("e")
    finally:
        out.append("outer_f")
    return out


def continue_try_loop_try_try():
    out = []
    try:
        for i in range(3):
            try:
                try:
                    1 / 0
                except ZeroDivisionError:
                    out.append("e" + str(i))
                    if i == 1:
                        continue
                finally:
                    out.append("inner_f" + str(i))
            except RuntimeError:
                out.append("mid_e")
            out.append("x" + str(i))
    finally:
        out.append("outer_f")
    return out


def return_try_loop_try_try():
    out = []
    def g():
        try:
            for i in range(3):
                try:
                    try:
                        out.append("t" + str(i))
                        if i == 1:
                            return "R"
                    finally:
                        out.append("inner_f" + str(i))
                except RuntimeError:
                    out.append("mid_e")
        finally:
            out.append("outer_f")
    return g(), out


def break_try_loop_except_as_finally():
    out = []
    try:
        for i in range(3):
            try:
                try:
                    1 / 0
                except ZeroDivisionError as e:
                    out.append("as" + str(i))
                    if i == 1:
                        break
                finally:
                    out.append("inner_f" + str(i))
            except RuntimeError:
                out.append("mid_e")
    except Exception:
        out.append("outer_e")
    finally:
        out.append("outer_f")
    return out


def break_nested_loops_try():
    out = []
    for i in range(3):
        try:
            for j in range(3):
                try:
                    try:
                        1 / 0
                    except ZeroDivisionError as e:
                        out.append(str(i) + str(j))
                        if j == 1:
                            break
                    finally:
                        out.append("if" + str(j))
                except RuntimeError:
                    pass
        finally:
            out.append("of" + str(i))
    return out


print(break_case())
print(continue_case())
print(return_case())
print(except_return_case())
print(break_in_try_except())
print(continue_in_try_except())
print(break_in_except_handler())
print(continue_in_except_handler())
print(break_in_except_as_handler())
print(continue_in_except_as_handler())
print(break_in_except_handler_with_finally())
print(continue_in_except_handler_with_finally())
print(break_in_except_as_with_finally())
print(nested_try_break())
print(return_in_try_except())
print(return_in_except_handler())
print(return_in_except_as_handler())
print(break_try_loop_try_try())
print(continue_try_loop_try_try())
print(return_try_loop_try_try())
print(break_try_loop_except_as_finally())
print(break_nested_loops_try())
