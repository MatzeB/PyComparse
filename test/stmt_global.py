counter = 13

def foo():
    counter = 1
    print(counter)

def bar():
    global counter
    counter += 1
    print(counter)

print("gcounter", counter)
foo()
print("gcounter", counter)
bar()
print("gcounter", counter)
