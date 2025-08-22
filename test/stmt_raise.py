try:
    raise Exception("jo")
except Exception as e:
    print(e)

try:
    raise 44
except Exception as e:
    print(e)

try:
    foo = 0
    try:
        foo / 0
    except:
        raise Exception("inner exc") from Exception("from exc")
except Exception as e:
    print(e.__cause__)
    print(e)
