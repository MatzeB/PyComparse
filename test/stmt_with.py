# Test with statement
class MockContextManager:
    def __init__(self, value):
        self.value = value
        
    def __enter__(self):
        print("Entering context with " + str(self.value))
        return self.value
        
    def __exit__(self, exc_type, exc_val, exc_tb):
        print("Exiting context with " + str(self.value))
        return False

# Basic with statement
with MockContextManager("test1"):
    print("Inside with block")

# With statement with 'as' clause
with MockContextManager("test2") as value:
    print("Value from context manager: " + str(value))

# Nested with statements
with MockContextManager("outer"):
    with MockContextManager("inner") as inner_value:
        print("Nested context: " + str(inner_value))