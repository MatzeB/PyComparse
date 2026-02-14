def outer_func_global():
    global func_global

    def func_global():
        return 1

    def nested_local():
        return 2

    return func_global, nested_local


def outer_class_global():
    global ClassGlobal

    class ClassGlobal:
        pass

    class LocalClass:
        pass

    return ClassGlobal, LocalClass


func_global_obj, nested_local_obj = outer_func_global()
class_global_obj, local_class_obj = outer_class_global()

print(func_global_obj.__qualname__)
print(func_global.__qualname__)
print(nested_local_obj.__qualname__)
print(class_global_obj.__qualname__)
print(ClassGlobal.__qualname__)
print(local_class_obj.__qualname__)
