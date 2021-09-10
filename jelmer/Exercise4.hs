
-- I was thinking of implementing a naive version of the sieve of Erathostenes as this would still be somewhat fast.
-- The first implementation I made requires a list of integers to be passed as argument, and loops over each element (the current index).
-- Now two things will happen:
-- 1. The current index will be added to the resulting list
-- 2. The remaining list will check for each element whether it is divisible by the index with remainder. If this is the case, the element will be removed from the resulting list.
-- The remaining list will contain all primes up until n.

-- There are many ways to improve this: we do not need to check divisibility any longer for numbers higher than sqrt(n) as one of factored numbers will be lower than it: meaning the number cannot be prime.

-- , starting with 2 up until n (10000 in this exercise), so we can use [2..10000].
getPrimes :: [Integer] -> [Integer]
getPrimes [] = [] -- Base case is required as this will recursively call getPrimes until each number is either added as a prime, or removed from the list due to being divisible by smaller number
getPrimes (x:xs) = x : getPrimes [i | i <- xs, (i `mod` x) /= 0]

-- This doesn't work because


-- getPrimesUpTo :: Integer -> [Integer]
-- getPrimesUpTo n = [2..n]

-- isPrime :: Integer -> Boolean
-- isPrime n = n 