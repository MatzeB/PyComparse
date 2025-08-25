from typing import List, Set

def no():
    pass
print(no.__annotations__)

def ret_int() -> int:
    return 0
print(ret_int.__annotations__)

def ret_none() -> None:
    pass
print(ret_none.__annotations__)

def args(x: int, y, z: float):
    pass
print(args.__annotations__)

def args_ret(x: float, /, *args: List[int], foo: bool=False) -> Set[bool]:
    return "wrong value; who cares"
print(args_ret.__annotations__)
