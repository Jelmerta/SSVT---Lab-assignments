
-- I was thinking of implementing a naive version of the sieve of Erathostenes as this would still be relatively fast, but this didn't work out as I don't know Haskell well enough to loop over the whole set
getPrimes :: [Integer] -> [Integer]
getPrimes [] = [] -- Base case is required as this will recursively call getPrimes until each number is either added as a prime, or removed from the list due to being divisible by smaller number
getPrimes (x:xs) = x : getPrimes [i | i <- xs, (i `mod` x) /= 0]

-- This doesn't work because


-- getPrimesUpTo :: Integer -> [Integer]
-- getPrimesUpTo n = [2..n]

-- isPrime :: Integer -> Boolean
-- isPrime n = n 