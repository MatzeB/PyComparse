# Unicode identifiers (PEP 3131)
café = "coffee"
naïve = "simple"
π = 3.14159
変数 = "variable"

print(café)
print(naïve)
print(π)
print(変数)

class 形状:
    def __init__(self, 名前):
        self.名前 = 名前
    def 表示(self):
        return self.名前

s = 形状("circle")
print(s.表示())

# Unicode keyword arg
def f(キー="default"):
    return キー
print(f())
print(f(キー="custom"))

# Unicode in comprehension and loop
結果 = [x for x in range(3)]
print(結果)

for 文字 in "ab":
    pass
print(文字)

# 蟒 = Python (the snake)
蟒 = "python"
print(蟒)

# NFKC normalization (PEP 3131)
# Micro sign (U+00B5) normalizes to Greek small mu (U+03BC)
µ = "micro"
print(μ)

# Math fraktur letters normalize to ASCII
𝔘𝔫𝔦𝔠𝔬𝔡𝔢 = "fraktur"
print(Unicode)

# NFD: e + combining acute composes to precomposed form
café = "nfd_coffee"
print(café)
