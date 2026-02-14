import math

# Overflow should produce infinities (with sign preserved).
for v in (1e425000000, -1e425000000):
    print(math.isinf(v), math.copysign(1.0, v))

# Underflow should produce signed zeros.
for v in (1e-425000000, -1e-425000000):
    print(v == 0.0, math.copysign(1.0, v))

# Complex literals with extreme values should also parse and preserve sign.
for z in (1e425000000j, -1e425000000j):
    print(z.real == 0.0, math.isinf(z.imag), math.copysign(1.0, z.imag))
