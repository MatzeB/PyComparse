class NullContext:
    def __enter__(self):
        return None
    def __exit__(self, a, b, c):
        return False

with NullContext():
    pass