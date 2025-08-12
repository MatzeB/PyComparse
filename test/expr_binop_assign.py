x = 17

x += 8
print(x)

a = [7] * 4
i = -1
a[i+3] ^= 3
print(a)

class C:
    pass
class V:
    def __matmul__(self, x):
        return "jo: " + str(x)
def nop(x):
    return x
c = C()
c.foobar = V()
nop(c).foobar @= 33
print(c.foobar)
