import dis


def f():
    if False:
        x = 1
        return x
    y = 2
    return y


for instruction in dis.get_instructions(f):
    print(instruction.opname, instruction.argval)
