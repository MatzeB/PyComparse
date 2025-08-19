def bar():
    pass

def test_try_except(x, t):
    try:
        if x > 0:
            x/0
        print("middle")
        if t:
            t.does_not_exist = 42
        print("end")
    except ZeroDivisionError as e:
        print("zero div error:" + str(e))
    except Exception as e2:
        print("generic exception" + str(e2))
    finally:
        print("finally")
    print("after")

print("# try-except")
test_try_except(0, False)
test_try_except(42, False)
test_try_except(0, True)
