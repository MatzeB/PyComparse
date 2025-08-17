def s(x):
    print("----")
    print(x)
    print(repr(x))
    print(len(x))
    print(type(x))

s("he'llo")
s(u"wo\"rld")
s(U"wo\'rld")
s(r"he\rllo")
s(R"jo\0jo")

s(b"jo\": \xfe")
s(b"jo\0jo")
s(rb"he\rllo")
s(rB"jo\0jo")
s(Rb"he\rllo")
s(RB"jo\0jo")
s(br"he\rllo")
s(bR"jo\0jo")
s(Br"he\rllo")
s(BR"jo\0jo")

u = 1
U = 2
r = 3
R = 4
b = 5
B = 6
print(u, U, r, R, b, B)

rb = 11
rB = 12
Rb = 13
RB = 14
print(rb, rB, Rb, RB)
