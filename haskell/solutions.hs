-- Problem 1
import Data.List (nub)
import Data.List.Ordered (minus)

multiples_of_three = [3,6..]
multiples_of_five = [5,10..]
multiples = (takeWhile (< 1000) multiples_of_three) ++ (takeWhile (< 1000) multiples_of_five)
solution_1 = sum (nub multiples)

solution_1' = sum [n | n <- [1..1000-1], n `mod` 5 == 0 || n `mod` 3 == 0]

-- Problem 2
fib :: Int -> Int -> [Int]
fib x y = x : y : fib' x y
    where fib' x' y' = (x' + y') : fib' y' (x' + y')

fib' :: Int -> Int -> [Int]
fib' x y = x : y : zipWith (+) (fib' x y) (tail (fib' x y))
-- This one is much slower for some reason

solution_2 = sum (filter even (takeWhile (< 4000000) (fib 1 2)))

-- Problem 3
primesTo m = sieve [3,5..m] where
    sieve (p:xs)
        | p*p > m = p : xs
        | otherwise = p : sieve (xs `minus` [p*p, p*p+2*p])

isFactor :: Int -> Int -> Bool
x `isFactor` y = y `mod` x == 0

isPrime :: Int -> Bool
isPrime x = not $ any divisible $ takeWhile notTooBig [2..]
    where divisible y = x `mod` y == 0
          notTooBig y = y * y <= x

-- Don't understand how this works
isPrime' n = n > 1 &&
              foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r))
                True primes

primes = 2 : filter isPrime [3,5..]

solution_3 = last $ filter isPrime $ filter (`isFactor` 600851475143) $ takeWhile notTooBig [2..]
    where notTooBig y = y * y <= 600851475143
