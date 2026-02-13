import dis


def plain():
    return {"a": 1, False: 2}


def mixed(key):
    return {key: 1, "b": 2}


def unpacked(extra):
    return {"a": 1, **extra, "b": 2, "c": 3}


print("plain")
dis.dis(plain)
print("mixed")
dis.dis(mixed)
print("unpacked")
dis.dis(unpacked)
