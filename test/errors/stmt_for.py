l = [1,2,3]
for i in *l:
    print(i)

l2 = {4:4}
for x in **l2:
    print(x)

l3 = [(1,2)]
for *x in l3:
    print(x)

for 5 in l:
    print("5")
