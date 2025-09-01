l = [(1,2,3), (4,5)]

for x, *rest in l:
    print(x)
    print(rest)

# The following is not allowed in 3.8 yet, but later...
#for x in 42, *range(3):
#    print(x)
