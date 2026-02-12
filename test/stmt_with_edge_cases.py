class SimpleContext:
    def __init__(self, name):
        self.name = name
    def __enter__(self):
        return self.name
    def __exit__(self, a, b, c):
        return False

# Single item (should still work)
with SimpleContext("single"):
    print("single item")

# Two items
with SimpleContext("first"), SimpleContext("second"):
    print("two items")

# Three items with mixed 'as' usage
with SimpleContext("a") as x, SimpleContext("b"), SimpleContext("c") as z:
    print("mixed: " + x + " and " + z)

# Track enter/exit calls for control flow tests
class TrackingContext:
    def __init__(self, name, out):
        self.name = name
        self.out = out
    def __enter__(self):
        self.out.append("enter " + self.name)
        return self
    def __exit__(self, *args):
        self.out.append("exit " + self.name)

# Break inside with
def with_break():
    out = []
    for i in range(3):
        with TrackingContext("a", out):
            out.append("body " + str(i))
            if i == 1:
                break
    return out

# Continue inside with
def with_continue():
    out = []
    for i in range(3):
        with TrackingContext("a", out):
            out.append("body " + str(i))
            if i == 1:
                continue
            out.append("after " + str(i))
    return out

# Return inside with
def with_return():
    out = []
    def g():
        with TrackingContext("a", out):
            out.append("body")
            return "R"
    return g(), out

# Break with multiple context managers
def with_multi_break():
    out = []
    for i in range(3):
        with TrackingContext("a", out), TrackingContext("b", out):
            out.append("body " + str(i))
            if i == 1:
                break
    return out

# Break inside with inside try/finally
def with_try_finally_break():
    out = []
    try:
        for i in range(3):
            with TrackingContext("a", out):
                out.append("body " + str(i))
                if i == 1:
                    break
    finally:
        out.append("outer_f")
    return out

# Break inside try/except inside with
def try_in_with_break():
    out = []
    for i in range(3):
        with TrackingContext("a", out):
            try:
                out.append("body " + str(i))
                if i == 1:
                    break
            except ValueError:
                pass
    return out

print(with_break())
print(with_continue())
print(with_return())
print(with_multi_break())
print(with_try_finally_break())
print(try_in_with_break())