# Basic closure over enclosing function variable
def outer():
    x = 10
    return lambda: x

print(outer()())

# Closure with lambda stored in local variable
def outer2():
    x = 20
    f = lambda: x
    return f

print(outer2()())

# Lambda with parameter and closure
def make_adder(n):
    return lambda x: x + n

add5 = make_adder(5)
print(add5(10))
print(add5(-3))

# Nested lambda closures
def chain():
    value = 42
    return lambda: (lambda: value)()

print(chain()())

# Multiple closures sharing the same cell variable
def multi():
    x = 100
    a = lambda: x + 1
    b = lambda: x + 2
    return a() + b()

print(multi())

# Lambda closing over loop variable (late binding)
def make_funcs():
    funcs = []
    for i in range(3):
        funcs.append(lambda: i)
    return funcs

print([f() for f in make_funcs()])

# Lambda with default capturing current value
def make_funcs_default():
    funcs = []
    for i in range(3):
        funcs.append(lambda i=i: i)
    return funcs

print([f() for f in make_funcs_default()])

# Lambda inside a class method
class MyClass:
    def method(self):
        x = 99
        return lambda: x

print(MyClass().method()())
