def foo(x):
    return "hello" if x == 0 else "world"

foo(0)
foo(-42)

def e(x):
    print("e(" + str(x) + ")")
    return x

def bar(x):
    return (e("fizz buzz") if e(x) % 3 == 0 and e(x) % 5 == 0 else 
            e("fizz") if e(x) % 3 == 0 else
            e("buzz") if e(x) % 5 == 0 else
            e(str(x)))

for i in range(20):
    bar(i)
