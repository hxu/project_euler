# Project Euler solution implementations
from __future__ import division
import cProfile
import math
import collections


def sieve(limit):
    return sieve_of_eratosthenes(limit)


def prod(iterable):
    from operator import mul
    return reduce(mul, iterable, 1)


def sieve_of_eratosthenes(limit):
    # Uses Sieve of Eratosthenes to get a list of primes up to limit
    # Pretty slow for larger limits, seems like the union is the problem?

    # from http://code.jasonbhill.com/python/project-euler-problem-7/
    # apparently printing is really slow

    # generate a list of Trues before starting the sieve
    primes = [True] * limit

    # 0 and 1 are not primes
    primes[0], primes[1] = [None] * 2

    for i, v in enumerate(primes):
        if v is True:
            # all multiples of that number are not prime
            primes[i*2::i] = [False] * (((limit - 1) // i) - 1)

    return [k for k, v in enumerate(primes) if v is True]


def iterable_sieve():
    """
    Iterable version of the sieve
    """
    excluded = []
    x = 2
    while True:
        prime = True
        for y in excluded:
            if x % y == 0:
                prime = False
                break
        if prime is True:
            yield x
        excluded.append(x)
        x += 1


def nth_prime(n):
    """
    Returns the nth prime
    """
    limit = n
    sieve = iterable_sieve()
    i = 0
    while i < limit:
        prime = sieve.next()
        i += 1
    return prime


def is_prime(num):
    """
    Check if a number is prime
    """
    if num % 2 == 0:
        return False

    fac = 3
    while fac < num**0.5 + 1:
        if num % fac == 0:
            return False
        fac += 2

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

#Solution5().run()


class Solution6(Solution):
    def sum_of_squares(self, *nums):
        return sum([x ** 2 for x in nums])

    def square_of_sum(self, *nums):
        return sum(nums) ** 2

    def solve(self, **kwargs):
        nums = range(1, 101)
        self.answer = self.square_of_sum(*nums) - self.sum_of_squares(*nums)

#Solution6().run()


class Solution7(Solution):
    def solve(self, limit=10001, **kwargs):
        self.answer = nth_prime(limit)

#Solution7().run()


class Solution8(Solution):
    def solve(self):

        num = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
        digits = [int(x) for x in str(num)]
        maximum = 0
        while len(digits) > 4:
            maximum = max(maximum, prod(digits[0:5]))
            digits.pop(0)

        self.answer = maximum

#Solution8().run()


class Solution9(Solution):
    #A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

    #a2 + b2 = c2
    #For example, 32 + 42 = 9 + 16 = 25 = 52.

    #There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    #Find the product abc.

    # a < b < c < 999
    # a + b + c = 1000
    # a2 + b2 = c2
    #
    # c = sqrt(a2 + b2) = 1000 - a - b
    # a + b = 1000 - c
    # c must be a perfect square as well, since it is the square root of two
    # integers
    # a + b + sqrt(a2 + b2) = 1000
    def is_perfect_square(self, num):
        return math.sqrt(num).is_integer()

    def pythagorean_triple(self, m, n):
        a = (m ** 2) - (n ** 2)
        b = 2 * m * n
        c = (m ** 2) + (n ** 2)
        return (a, b, c)

    def solve(self):
        self.answer = None
        # find all perfect squares less than 999
#        cs = [x for x in range(1, 1000) if self.is_perfect_square(x)]
        m = 2
        while self.answer is None:
            for n in range(1, m):
                triple = self.pythagorean_triple(m, n)
                print(triple)
                if sum(triple) == 1000:
                    self.answer = prod(triple)
            m += 1


#Solution9().run()

class Solution10(Solution):
    def solve(self):
        primes = sieve_of_eratosthenes(2000001)
        self.answer = sum(primes)

Solution10().run()
