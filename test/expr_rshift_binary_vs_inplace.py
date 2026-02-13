class ShiftOps:
    def __rshift__(self, other):
        return "binary"

    def __irshift__(self, other):
        return "inplace"


value = ShiftOps()
print(value >> 1)

value = ShiftOps()
value >>= 1
print(value)
