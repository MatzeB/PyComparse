x = list(range(10))
print(x[:])
print(x[::])
print(x[2:])
print(x[2::])
print(x[:9:3])
print(x[::3])
print(x[:9:])
print(x[2:9:3])
x[1:5:2] = 5,99
print(x)

class C:
    def __getitem__(self, x):
        print(x)
        return 42
    def __setitem__(self, x, value):
        print(x)
        print(value)

c = C()
print(c["batman":"WUT"])
print(c[2:4,2::-1,"hello",:])
c["jo",None] = True
