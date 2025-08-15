class TestContext:
    def __init__(self, name):
        self.name = name
        
    def __enter__(self):
        print("entering " + self.name)
        return self.name
        
    def __exit__(self, a, b, c):
        print("exiting " + self.name)
        return False

# Multiple with items without 'as' clause
with TestContext("first"), TestContext("second"):
    print("in both contexts")

# Multiple with items with 'as' clauses
with TestContext("alpha") as a, TestContext("beta") as b:
    print("got: " + a + " and " + b)

# Mixed with and without 'as' clauses
with TestContext("one"), TestContext("two") as t:
    print("got: " + t)