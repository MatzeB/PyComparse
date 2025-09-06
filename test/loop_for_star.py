l = [(1,2,3), (4,5)]

for x, *rest in l:
    print(x)
    print(rest)

# strictly speaking this is not allowed in 3.8...
# import sys
# if sys.version_info >= (3,9):
#     for x in 42, *range(3):
#         print(x)
#     for x in *range(2), 33:
#        print(x)
