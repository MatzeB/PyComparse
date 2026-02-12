from __future__ import annotations

def side():
    print("side")
    return int

sx: side()
print("module", __annotations__)

class C:
    cx: side()

    def m(self, a: side()) -> side():
        return 0

print("class", C.__annotations__)
print("func", C.m.__annotations__)
print("flags", bool(C.m.__code__.co_flags & 0x1000000))

o = C()
o.attr: side()
lst = [0]
lst[0]: side()
