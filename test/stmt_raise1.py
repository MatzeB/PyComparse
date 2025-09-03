def foo():
    try:
        raise Exception("jo")
    except:
        raise

try:
    foo()
except Exception as e:
    print("catched", e)
