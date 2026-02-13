def dis(x=None, *, file=None, depth=None):
    print("vals", x, file, depth)


print(dis.__defaults__)
print(dis.__kwdefaults__)
print(dis.__code__.co_argcount, dis.__code__.co_kwonlyargcount)

dis()
dis(1)
dis(file="f")
dis(depth=9)
dis(2, file="x", depth=3)

try:
    dis(1, 2)
except TypeError as error:
    print(type(error).__name__)
