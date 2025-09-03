#!/usr/bin/env python3

indent = ""
for _ in range(101):
    print(f"{indent}if True:")
    indent += "  "
print(f"{indent}pass")
