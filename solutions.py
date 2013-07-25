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
    # uses sieve of eratosthenes to get a list of primes up to limit
    # pretty slow for larger limits, seems like the union is the problem?

    # from http://code.jasonbhill.com/python/project-euler-problem-7/
    # apparently printing is really slow

    # generate a list of trues before starting the sieve
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

#Solution10().run()

class Solution11(Solution):
    def solve(self):
        matrix_string = "8 2 22 97 38 15 0 40 0 75 4 5 7 78 52 12 50 77 91 8; 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 4 56 62 0; 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 3 49 13 36 65; 52 70 95 23 4 60 11 42 69 24 68 56 1 32 56 71 37 2 36 91; 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80; 24 47 32 60 99 3 45 2 44 75 33 53 78 36 84 20 35 17 12 50; 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70; 67 26 20 68 2 62 12 20 95 63 94 39 63 8 40 91 66 49 94 21; 24 55 58 5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72; 21 36 23 9 75 0 76 44 20 45 35 14 0 61 33 97 34 31 33 95; 78 17 53 28 22 75 31 67 15 94 3 80 4 62 16 14 9 53 56 92; 16 39 5 42 96 35 31 47 55 58 88 24 0 17 54 24 36 29 85 57; 86 56 0 48 35 71 89 7 5 44 44 37 44 60 21 58 51 54 17 58; 19 80 81 68 5 94 47 69 28 73 92 13 86 52 17 77 4 89 55 40; 4 52 8 83 97 35 99 16 7 97 57 32 16 26 26 79 33 27 98 66; 88 36 68 87 57 62 20 72 3 46 33 67 46 55 12 32 63 93 53 69; 4 42 16 73 38 25 39 11 24 94 72 18 8 46 29 32 40 62 76 36; 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 4 36 16; 20 73 35 29 78 31 90 1 74 31 49 71 48 86 81 16 23 57 5 54; 1 70 54 71 83 51 54 69 16 92 33 48 61 43 52 1 89 19 67 48"
        import numpy
        matrix = numpy.matrix(matrix_string)
        maxes = []

        # horizontal products
        h_prods = []
        for y in range(0, 20):
            for x in range(0, 16):
                h_prods.append(matrix[y, x:x+4].sum())

        maxes.append(max(h_prods))

        # vertical products
        v_prods = []
        for x in range(0, 20):
            for y in range(0, 16):
                v_prods.append(matrix[y:y+4, x].sum())
        maxes.append(max(v_prods))

        # \ diagonal products
        d_prods = []
        for x in range(0, 16):
            for y in range(0, 16):
                d_prods.append(matrix[x,y] * matrix[x+1, y+1] * matrix[x+2, y+2] * matrix[x+3, y+3])
        maxes.append(max(d_prods))

        # / diagonal products
        d2_prods = []
        for x in range(0, 16):
            for y in range(3, 20):
                d2_prods.append(matrix[y,x] * matrix[y-1, x+1] * matrix[y-2, x+2] * matrix[y-3, x+3])
        maxes.append(max(d2_prods))

        self.answer = (max(maxes))

#Solution11().run()

class Solution12(Solution):
    def triangular_number(self, n):
        return sum(range(1, n+1))

    def factors(self, n):
        # Returns list of factors of n
        factors = [1, n]
        x = 2
        while x < ((n ** 0.5) + 1):
            if n % x == 0:
                factors.extend([x, n / x])
            x += 1

        return factors

    def solve(self):
        x = 1
        factors = []
        while len(factors) <= 500:
            num = self.triangular_number(x)
            factors = self.factors(num)
            x += 1
        self.answer = num

#Solution12().run()

class Solution13(Solution):
    def solve(self):
        nums = [37107287533902102798797998220837590246510135740250, 46376937677490009712648124896970078050417018260538, 74324986199524741059474233309513058123726617309629, 91942213363574161572522430563301811072406154908250, 23067588207539346171171980310421047513778063246676, 89261670696623633820136378418383684178734361726757, 28112879812849979408065481931592621691275889832738, 44274228917432520321923589422876796487670272189318, 47451445736001306439091167216856844588711603153276, 70386486105843025439939619828917593665686757934951, 62176457141856560629502157223196586755079324193331, 64906352462741904929101432445813822663347944758178, 92575867718337217661963751590579239728245598838407, 58203565325359399008402633568948830189458628227828, 80181199384826282014278194139940567587151170094390, 35398664372827112653829987240784473053190104293586, 86515506006295864861532075273371959191420517255829, 71693888707715466499115593487603532921714970056938, 54370070576826684624621495650076471787294438377604, 53282654108756828443191190634694037855217779295145, 36123272525000296071075082563815656710885258350721, 45876576172410976447339110607218265236877223636045, 17423706905851860660448207621209813287860733969412, 81142660418086830619328460811191061556940512689692, 51934325451728388641918047049293215058642563049483, 62467221648435076201727918039944693004732956340691, 15732444386908125794514089057706229429197107928209, 55037687525678773091862540744969844508330393682126, 18336384825330154686196124348767681297534375946515, 80386287592878490201521685554828717201219257766954, 78182833757993103614740356856449095527097864797581, 16726320100436897842553539920931837441497806860984, 48403098129077791799088218795327364475675590848030, 87086987551392711854517078544161852424320693150332, 59959406895756536782107074926966537676326235447210, 69793950679652694742597709739166693763042633987085, 41052684708299085211399427365734116182760315001271, 65378607361501080857009149939512557028198746004375, 35829035317434717326932123578154982629742552737307, 94953759765105305946966067683156574377167401875275, 88902802571733229619176668713819931811048770190271, 25267680276078003013678680992525463401061632866526, 36270218540497705585629946580636237993140746255962, 24074486908231174977792365466257246923322810917141, 91430288197103288597806669760892938638285025333403, 34413065578016127815921815005561868836468420090470, 23053081172816430487623791969842487255036638784583, 11487696932154902810424020138335124462181441773470, 63783299490636259666498587618221225225512486764533, 67720186971698544312419572409913959008952310058822, 95548255300263520781532296796249481641953868218774, 76085327132285723110424803456124867697064507995236, 37774242535411291684276865538926205024910326572967, 23701913275725675285653248258265463092207058596522, 29798860272258331913126375147341994889534765745501, 18495701454879288984856827726077713721403798879715, 38298203783031473527721580348144513491373226651381, 34829543829199918180278916522431027392251122869539, 40957953066405232632538044100059654939159879593635, 29746152185502371307642255121183693803580388584903, 41698116222072977186158236678424689157993532961922, 62467957194401269043877107275048102390895523597457, 23189706772547915061505504953922979530901129967519, 86188088225875314529584099251203829009407770775672, 11306739708304724483816533873502340845647058077308, 82959174767140363198008187129011875491310547126581, 97623331044818386269515456334926366572897563400500, 42846280183517070527831839425882145521227251250327, 55121603546981200581762165212827652751691296897789, 32238195734329339946437501907836945765883352399886, 75506164965184775180738168837861091527357929701337, 62177842752192623401942399639168044983993173312731, 32924185707147349566916674687634660915035914677504, 99518671430235219628894890102423325116913619626622, 73267460800591547471830798392868535206946944540724, 76841822524674417161514036427982273348055556214818, 97142617910342598647204516893989422179826088076852, 87783646182799346313767754307809363333018982642090, 10848802521674670883215120185883543223812876952786, 71329612474782464538636993009049310363619763878039, 62184073572399794223406235393808339651327408011116, 66627891981488087797941876876144230030984490851411, 60661826293682836764744779239180335110989069790714, 85786944089552990653640447425576083659976645795096, 66024396409905389607120198219976047599490197230297, 64913982680032973156037120041377903785566085089252, 16730939319872750275468906903707539413042652315011, 94809377245048795150954100921645863754710598436791, 78639167021187492431995700641917969777599028300699, 15368713711936614952811305876380278410754449733078, 40789923115535562561142322423255033685442488917353, 44889911501440648020369068063960672322193204149535, 41503128880339536053299340368006977710650566631954, 81234880673210146739058568557934581403627822703280, 82616570773948327592232845941706525094512325230608, 22918802058777319719839450180888072429661980811197, 77158542502016545090413245809786882778948721859617, 72107838435069186155435662884062257473692284509516, 20849603980134001723930671666823555245252804609722, 53503534226472524250874054075591789781264330331690]
        res = sum(nums)
        self.answer = str(res)[0:10]

#Solution13().run()

class Solution14(Solution):

    def solve(self):
        collantz_cache = {}

        max_length = 1
        seed = 1
        while seed < 1000000:
            num = seed
            count = 1
            while num != 1:
                if collantz_cache.get(num, None):
                    count += collantz_cache[num]
                    num = 1
                elif num % 2 == 0:
                    num = num / 2
                else:
                    num = (3 * num) + 1
                count += 1
            collantz_cache[seed] = count
            if count > max_length:
                print 'new length {} with seed {}'.format(count, seed)
                max_length = count
            seed += 1
        self.answer = max_length

# Solution14().run()

class Solution15(Solution):
    def solve(self):
        from operator import mul
        import math
        # have to go right 20 times, then down 20 times.  So should be 40! / 20!*20!
        top = reduce(mul, range(21, 41), 1)
        bottom = math.factorial(20)
        self.answer = long(top / bottom)

# Solution15().run()

class Solution16(Solution):
    def solve(self):
        val = 2 ** 1000
        digits = [int(x) for x in str(val)]
        self.answer = sum(digits)

# Solution16().run()

class Solution17(Solution):
    def solve(self):

        def number_to_word(num):
            words = {
                1: 'one',
                2: 'two',
                3: 'three',
                4: 'four',
                5: 'five',
                6: 'six',
                7: 'seven',
                8: 'eight',
                9: 'nine',
                10: 'ten',
                11: 'eleven',
                12: 'twelve',
                13: 'thirteen',
                14: 'fourteen',
                15: 'fifteen',
                16: 'sixteen',
                17: 'seventeen',
                18: 'eighteen',
                19: 'nineteen',
            }
            tens = {
                20: 'twenty',
                30: 'thirty',
                40: 'forty',
                50: 'fifty',
                60: 'sixty',
                70: 'seventy',
                80: 'eighty',
                90: 'ninety'
            }
            places = {
                100: 'hundred',
                1000: 'thousand'
            }
            # strategy is to divide the number from left to right and try to match it to a word
            orig_num = num
            word = ''

            if num == 1000:
                # since max out at 1000, we know it's definitely 1000
                word += 'onethousand'
                num -= 1000

            if bool(num // 100):
                hundreds = num // 100
                word += words[hundreds] + 'hundred'
                num -= hundreds * 100
                if num:
                    word += 'and'

            if num >= 20:
                keys = tens.keys()
                while keys:
                    ten = keys.pop()
                    if num // ten == 1 and num % ten < 10:
                        word += tens[ten]
                        num -= ten
                        break
                if num:
                    word += words[num]
            else:
                if num:
                    word += words[num]

            return word

        nums = range(1, 1001)
        worded_nums = dict((n, number_to_word(n)) for n in nums)
        wrd = ''.join(worded_nums.values())
        print worded_nums

        print len(''.join([x[1] for x in worded_nums.items() if x[0] < 10]))
        print len(''.join([x[1] for x in worded_nums.items() if x[0] >= 10 and x[0] < 20]))
        print len(''.join([x[1] for x in worded_nums.items() if x[0] >= 20 and x[0] < 100]))
        print len(''.join([x[1] for x in worded_nums.items() if x[0] >= 100 and x[0] < 1000]))

        self.answer = len(wrd)

# Solution17().run()

class Solution18(Solution):
    triangle = \
        """75
            95 64
            17 47 82
            18 35 87 10
            20 04 82 47 65
            19 01 23 75 03 34
            88 02 77 73 07 63 67
            99 65 04 28 06 16 70 92
            41 41 26 56 83 40 80 70 33
            41 48 72 33 47 32 37 16 94 29
            53 71 44 65 25 43 91 52 97 51 14
            70 11 33 28 77 73 17 78 39 68 17 57
            91 71 52 38 17 14 91 43 58 50 27 29 48
            63 66 04 68 89 53 67 30 73 16 69 87 40 31
            04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"""

    def solve_from_top(self):
        rows = [[int(y) for y in x.strip().split(" ")] for x in self.triangle.split("\n")]
        num_rows = len(rows)
        maximum = 0
        paths = {}  # keys are strings of indices at each level
        for lev in range(0, num_rows):
            print "now on row {}".format(lev)
            row = rows[lev]
            if lev == 0:
                # special case of first row
                paths["0"] = row[0]
                maximum = row[0]
            else:
                for x in range(0, len(row)):
                    priors = []
                    for y in paths.keys():
                        path = y.split(",")
                        if len(path) == lev:
                            if y[-1] in [str(x), str(x-1)]:
                                priors.append(y)
                    for y in priors:
                        new_path = y + ",{}".format(x)
                        new_sum = paths[y] + row[x]
                        paths[new_path] = new_sum
                        if new_sum > maximum:
                            maximum = new_sum
                            print "new max {}".format(maximum)
        self.answer = maximum

    def solve(self):
        # More efficient methodology going from the bottom up
        sums = []
        rows = [[int(y) for y in x.strip().split(" ")] for x in self.triangle.split("\n")]
        rows.reverse()
        for r in rows:
            if len(sums) == 0:
                sums.append(r)
            else:
                last_row = sums[-1]
                new_row = []
                for i, x in enumerate(r):
                    new_row.append(max(x + last_row[i], x + last_row[i+1]))
                sums.append(new_row)
                self.answer = max(new_row)

# Solution18().run()

class Solution19(Solution):
    def solve(self):
        from datetime import date, timedelta
        last_date = date(2000, 12, 31)
        first_sunday = date(1901, 1, 6)
        week = timedelta(7)
        curr_date = first_sunday
        cnt = 0
        while curr_date <= last_date:
            if curr_date.day == 1:
                cnt += 1
            curr_date += week

        self.answer = cnt

# Solution19().run()

class Solution20(Solution):
    def solve(self):
        import math
        fac = math.factorial(100)
        digits = [int(x) for x in list(str(fac))]
        self.answer = sum(digits)

# Solution20().run()

class Solution21(Solution):
    def solve(self):
        amicable_numbers = []
        nums = range(1, 10001)
        while nums:
            x = nums.pop(0)
            sum_of_div = self.sum_of_factors(x)
            if self.sum_of_factors(sum_of_div) == x and sum_of_div != x:
                amicable_numbers.extend([sum_of_div, x])

        print amicable_numbers
        print set(amicable_numbers)
        self.answer = sum(set(amicable_numbers))

    def sum_of_factors(self, n):
        # Returns sum of factors of n
        # from solution 12, but excluding the number itself
        factors = [1]
        x = 2
        while x < ((n ** 0.5) + 1):
            if n % x == 0:
                factors.extend([x, n / x])
            x += 1
        return sum(factors)

# Solution21().run()

class Solution22(Solution):
    def solve(self):
        import string
        names_file = open('names.txt', 'r')
        names = [x.strip('"') for x in names_file.read().split(",")]
        names.sort()
        letters = list(string.ascii_uppercase)
        scores = []
        for i, n in enumerate(names):
            scores.append(sum([(letters.index(x) + 1) for x in n]) * (i + 1))
            if n == "COLIN":
                print i

        self.answer = sum(scores)

# Solution22().run()

class Solution23(Solution):
    def solve(self):
        upper_limit = 28123
        numbers = range(1, upper_limit + 1)
        res = []
        for n in numbers:
            for x in range(1, n):
                first = self.sum_of_factors(x)
                diff = n - x
                second = self.sum_of_factors(diff)
                if first > x and second > diff:
                    print "{} is a sum of abundant numbers {} and {}".format(n, x, diff)
                    res.append(n)
                    break

        self.answer = sum(set(res))

    def sum_of_factors(self, n):
        # Returns sum of factors of n
        # from solution 12, but excluding the number itself
        factors = [1]
        x = 2
        while x < ((n ** 0.5) + 1) and x != n:
            if n % x == 0:
                factors.extend([x, n / x])
            x += 1
        return sum(set(factors))


Solution23().run()