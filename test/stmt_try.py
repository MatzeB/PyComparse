def bar():
    pass

def test_try_except(x):
    try:
        if x > 0:
            x/0
        bar()
    except ZeroDivisionError:
        print("except")
    except 55:
        print("huh... 55")
    print("after")

print("# try-except")
test_try_except(0)
test_try_except(42)

def test_try_except_finally(x):
    try:
        if x > 0:
            x/0
        bar()
    except ZeroDivisionError:
        print("except")
    finally:
        print("finally")
    print("after")

print("# try-except-finally")
test_try_except_finally(0)
test_try_except_finally(5)

def test_try_except_else(x):
    try:
        if x > 0:
            x/0
        bar()
    except ZeroDivisionError:
        print("except")
    else:
        print("else")
    print("after")

print("# try-except-else")
test_try_except_else(0)
test_try_except_else(5)

def test_try_except_else_finally(x):
    try:
        if x > 0:
            x/0
        bar()
    except ZeroDivisionError:
        print("except")
    else:
        print("else")
    finally:
        print("finally")
    print("after")

print("# try-except-else-finally")
test_try_except_else_finally(0)
test_try_except_else_finally(5)

def test_try_finally(x):
    try:
        if x > 0:
            x/0
        bar()
    finally:
        print("finally")
    print("after")

print("# try-finally")
test_try_finally(0)
try:
    test_try_finally(5)
except:
    pass
