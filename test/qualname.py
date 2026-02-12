def outer():
    def inner():
        def deep():
            pass
        print(deep.__qualname__)
    inner()
    x = lambda: None
    print(inner.__qualname__)
    print(x.__qualname__)

outer()

class MyClass:
    def method(self):
        def nested():
            pass
        print(nested.__qualname__)
    x = lambda: None

m = MyClass()
m.method()
print(MyClass.method.__qualname__)
print(MyClass.x.__qualname__)

class Outer:
    class Inner:
        def method(self):
            pass

print(Outer.Inner.__qualname__)
print(Outer.Inner.method.__qualname__)

def f():
    x = [i for i in range(3)]
    print(x)

f()
