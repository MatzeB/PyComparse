# Test basic super().__init__
class A:
    def __init__(self):
        self.x = 10

class B(A):
    def __init__(self):
        super().__init__()
        self.y = 20

b = B()
print(b.x, b.y)

# Test super() in regular method
class C:
    def greet(self):
        return "hello"

class D(C):
    def greet(self):
        return super().greet() + " world"

print(D().greet())

# Test super() with explicit arguments
class E:
    def value(self):
        return 42

class F(E):
    def value(self):
        return super(F, self).value() + 1

print(F().value())

# Test super() in __new__
class G:
    def __new__(cls):
        instance = super().__new__(cls)
        instance.created = True
        return instance

print(G().created)

# Test multi-level inheritance
class H:
    def val(self):
        return 1

class I(H):
    def val(self):
        return super().val() + 10

class J(I):
    def val(self):
        return super().val() + 100

print(J().val())

# Test super() in classmethod
class K:
    @classmethod
    def who(cls):
        return "K"

class L(K):
    @classmethod
    def who(cls):
        return super().who() + "L"

print(L.who())

# Test super() accessing attribute
class M:
    x = 99

class N(M):
    def get_x(self):
        return super().x

print(N().get_x())

# Test diamond inheritance with super()
class P:
    def who(self):
        return ["P"]

class Q(P):
    def who(self):
        return super().who() + ["Q"]

class R(P):
    def who(self):
        return super().who() + ["R"]

class S(Q, R):
    def who(self):
        return super().who() + ["S"]

print(S().who())

# Test super assigned to a variable
class A1:
    def method(self):
        return "A1"

class B1(A1):
    def method(self):
        s = super()
        return s.method()

print(B1().method())

# Test multiple methods using super in same class
class A4:
    def foo(self):
        return "foo"
    def bar(self):
        return "bar"

class B4(A4):
    def foo(self):
        return super().foo() + "!"
    def bar(self):
        return super().bar() + "!"

print(B4().foo(), B4().bar())

# Test super() combined with explicit __class__
class A8:
    pass

class B8(A8):
    def info(self):
        return (super().__class__.__name__, __class__.__name__)

print(B8().info())
