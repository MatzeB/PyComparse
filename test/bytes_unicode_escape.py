# In bytes literals, \u and \U are NOT unicode escapes — they are kept
# as literal backslash + letter, just like \N is.

print(b"\u26f7")          # b'\\u26f7'
print(b"\U0001F600")      # b'\\U0001F600'
print(b"\u0041")          # b'\\u0041'  (NOT b'A')
print(b"\U00000041")      # b'\\U00000041'

# Mix with valid byte escapes to confirm those still work.
print(b"\x41\u0041")      # b'A\\u0041'
print(b"\n\u000a")        # b'\n\\u000a'

# \N is similarly not a named-escape in bytes literals.
print(b"\N{SNOWMAN}")     # b'\\N{SNOWMAN}'

# In regular strings \u/\U/\N ARE processed.
print("\u26f7")           # ⛷
print("\U0001F600")       # 😀
