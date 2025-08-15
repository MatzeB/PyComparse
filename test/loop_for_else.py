def foo(x):
    for i in range(x):
        print(i)
        if i > 4:
            break
    else:
        print("else")
    print(".")

foo(4)
foo(8)
