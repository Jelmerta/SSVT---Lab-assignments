-- Exercise 4
-- The natural number 13 has the property that it is prime and its reversal, the number 31, is also prime. Write a function that finds all primes < 10000 with this property.

-- To do this we split up the exercise in two parts:
-- First we want to have a list of all primes up to 10000
-- Second: For all those primes, we want to find which primes are the same when reversed

-- Finding primes:
-- I was thinking of implementing a naive version of the sieve of Erathostenes as this would still be somewhat fast.
-- The first implementation I made requires a list of integers to be passed as argument, and loops over each element (the current index).
-- Now two things will happen:
-- 1. The current index will be added to the resulting list
-- 2. The remaining list will check for each element whether it is divisible by the index with remainder. If this is the case, the element will be removed from the resulting list.
-- The remaining list will contain all primes up until n.

-- There are many ways to improve this: we do not need to check divisibility any longer for numbers higher than sqrt(n) as one of factored numbers will be lower than it: meaning the number cannot be prime.
-- TODO Other ways to improve

-- starting with 2 up until n (10000 in this exercise), so we can use [2..10000].
getPrimes :: [Integer] -> [Integer]
getPrimes [] = [] -- Base case is required as this will recursively call getPrimes until each number is either added as a prime, or removed from the list due to being divisible by smaller number
getPrimes (x:xs) = x : getPrimes [i | i <- xs, (i `mod` x) /= 0]


-- Reversable primes:
-- We can use the code from the exercise to reverse a number

reversal :: Integer -> Integer
reversal = read . reverse . show


-- For every prime in the list, we want to check if the reversal of that number is also in that same list.
-- It might be noteworthy that some prime reversals will lead to the same digit, instead of a different prime, such as single digit primes, and 11.
-- These could easily be filtered out by adding a condition that the number and the reversal should not be the same.
getReversiblePrime :: [Integer] -> [Integer]
getReversiblePrime xs = [x | x <- xs, reversal x `elem` xs]


-- Bringing it all together:
getReversiblePrimeUpTo10000 :: [Integer]
getReversiblePrimeUpTo10000 = getReversiblePrime (getPrimes [2..10000])