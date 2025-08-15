# Simple with statement test
class SimpleContext:
    def __enter__(self):
        print("entering")
        return 42
        
    def __exit__(self, a, b, c):
        print("exiting")
        return False

# Basic with statement
with SimpleContext():
    print("in with block")

# With statement with 'as' clause  
with SimpleContext() as x:
    print(x)