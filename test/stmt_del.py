a = 1
b = 1
c = 1
d = 1
e = 1
dir()

del c
dir()

del a, (b, e)
dir()

arr = [1,2,33,44,555,-66,7777]
del arr[3]
print(arr)

class C:
    pass

obj = C()
obj.foobar = 42
obj.baz = True

del [obj.baz, d, arr[1]]
print(arr)
dir(obj)
dir()
