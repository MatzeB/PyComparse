source = """
def generated():
    return "AllTests"
"""

namespace = {}
exec(source, namespace, namespace)
print(namespace["generated"]())
