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