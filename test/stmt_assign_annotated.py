from typing import Dict, List

def announced_float():
    print("getting float type")
    return float
hahaha = object
class C: pass
obj = C()

# Module scope
ivar: int = 42
fvar: announced_float() = 66

mydata: Dict[str, List[bool]]

arr = list(range(10))
arr[2] : str = "jo"
arr[3] : announced_float()

obj.foo : str = "jo"
obj.foo : announced_float() = 42.4
only_in_global: int = 1
print(__annotations__)

# Class scope
class MyClass:
    ivar: int = 42
    fvar: announced_float() = 66

    mydata: Dict[str, List[bool]]

    arr = list(range(10))
    arr[2] : str = "jo"
    arr[3] : announced_float()

    obj.foo : str = "jo"
    obj.foo : announced_float() = 42.4

    only_in_class: bool = True

    print(__annotations__)

# Function scope
def func():
    ivar: int = 42
    fvar: announced_float() = 66

    mydata: Dict[str, List[bool]]

    arr = list(range(10))
    arr[2] : str = "jo"
    arr[3] : announced_float()

    obj.foo : str = "jo"
    obj.foo : announced_float() = 42.4

func()
