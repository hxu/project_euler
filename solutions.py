# Project Euler solution implementations
from __future__ import division
import cProfile
import math
import collections


def counter(limit):
    x = 1
    while x < limit:
        yield x
        x += 1


def multiplier(limit, by):
    x = 1
    y = x * by
    while y < limit:
        yield x
        y += by


def sieve(limit):
    return sieve_of_eratosthenes(limit)


def sieve_of_eratosthenes(limit):
    # Uses Sieve of Eratosthenes to get a list of primes up to limit
    # Pretty slow for larger limits, seems like the union is the problem?
    nums = range(2, limit)
    excluded = set([1])
    primes = [1]
    for x in nums:
        if x in excluded:
            print("skipping {}".format(x))
            continue
        primes.append(x)
        print("primes {} added".format(x))
        excluded = excluded.union(set(range(x, limit, x)))
    return primes


def is_prime(num):
    """
    Check if a number is prime
    """
    for x in range(2, int(math.ceil(math.sqrt(num))) + 1):
        if num % x == 0:
            return False
    return True


def is_palindromic(num):
    """
    Checks if a number reads both the same both ways
    """
    if isinstance(num, collections.deque):
        remaining = num
    else:
        remaining = collections.deque(str(num))
    if len(remaining) in [0, 1]:
        return True
    else:
        left = remaining.popleft()
        right = remaining.pop()
        if left == right:
            return is_palindromic(remaining)
        else:
            return False


def gcd(*numbers):
    """
    https://gist.github.com/endolith/114336
    """
    from fractions import gcd
    return reduce(gcd, numbers)


def lcm(*numbers):
    """
    Returns the least common multiple of two numbers
    """
    return reduce(lambda x, y: (x * y) / gcd(x, y), numbers, 1)


class Solution(object):
    def run(self, **kwargs):
        print("Running {}".format(self.__class__.__name__))
        cProfile.runctx('self.solve(**kwargs)', globals(), locals())
        print("The solution is {}".format(self.answer))

    def solve(self):
        raise NotImplemented


class Solution1(Solution):
    def solve(self, **kwargs):
        # If we list all the natural numbers below 10 that are multiples of 3 or 5,
        # we get 3, 5, 6 and 9. The sum of these multiples is 23.

        # Find the sum of all the multiples of 3 or 5 below 1000.
        mult = [3 * x for x in range(1, (1000 / 3) + 1)]
        mult.extend([5 * x for x in range(1, 1000 / 5)])
        nums = set(mult)
        self.answer = sum(nums)

#Solution1().run()


class Solution2(Solution):
    def solve(self, **kwargs):
        # Each new term in the Fibonacci sequence is generated by adding the previous two terms.
        # By starting with 1 and 2, the first 10 terms will be:

#            1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

        # By considering the terms in the Fibonacci sequence whose values do not exceed four million,
        # find the sum of the even-valued terms.
        limit = kwargs['limit']
        fib = [1, 2]
        while fib[len(fib) - 1] < limit:
            fib.append(fib[len(fib) - 1] + fib[len(fib) - 2])
        res = [x for x in fib if x % 2 == 0]
        self.answer = sum(res)

#Solution2().run(limit=4000000)


class Solution3(Solution):
    def solve(self, **kwargs):
        limit = kwargs['limit']
        primes = []
        for x in range(2, int(math.ceil(math.sqrt(limit)))):
            if is_prime(x):
                if limit % x == 0:
                    primes.append(x)
                    self.answer = x
        print(primes)
        return primes

#Solution3().run(limit=600851475143)


class Solution4(Solution):
    def solve(self, **kwargs):
        three_digit = range(900, 1000)
        nums = set()
        for x in three_digit:
            for y in three_digit:
                nums.add(x * y)
        ans = 0
        for x in nums:
            if is_palindromic(x) and x > ans:
                ans = x
        self.answer = ans

#Solution4().run()


class Solution5(Solution):
    def solve(self, **kwargs):
        self.answer = lcm(*range(1, 21))

Solution5().run()
