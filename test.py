
def factors(n):
    # Returns list of factors of n
    factors = [1, n]
    x = 2
    while x < ((n ** 0.5) + 1):
        print x
        if n % x == 0:
            factors.extend([x, n / x])
            print factors
        x += 1
    return factors

print factors(28)
