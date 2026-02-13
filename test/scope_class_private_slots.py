class Rat:
    __slots__ = ["_Rat__num"]

    def __init__(self, value):
        self.__num = value

    def get(self):
        return self.__num


instance = Rat(7)
print(instance.get())
print(hasattr(instance, "_Rat__num"))
