def outer_class_body_reads_enclosing():
    x = 41

    class C:
        y = x + 1

    return C.y


print(outer_class_body_reads_enclosing())


def outer_method_reads_enclosing():
    x = 9

    class C:
        def method(self):
            return x

    return C().method()


print(outer_method_reads_enclosing())


def class_local_does_not_shadow_method_closure():
    x = 1

    class C:
        x = 20

        def method(self):
            return x

    return C().method()


print(class_local_does_not_shadow_method_closure())


def enclosing_class_local_does_not_shadow_nested_class():
    x = 3

    class C:
        x = 100

        class D:
            y = x

    return C.D.y


print(enclosing_class_local_does_not_shadow_nested_class())


x = "module"


def class_global_statement_only_applies_to_class_body():
    x = "enclosing"

    class C:
        global x
        seen = x

        def method(self):
            return x

    return C.seen, C().method()


print(class_global_statement_only_applies_to_class_body())


def class_local_blocks_class_deref_load():
    blocked = 7
    try:
        class C:
            y = blocked
            blocked = 2
    except NameError as e:
        return type(e).__name__
    return "unexpected"


print(class_local_blocks_class_deref_load())
