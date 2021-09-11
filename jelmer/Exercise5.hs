import Lab1
-- Exercise 5
-- Find the smallest prime number that is a sum of 101 consecutive primes.

-- Again, we need to first have a list of primes that will contain this solution. This might require a large amount of primes as there might be larger primes that when summing will still lead to a smaller number. 
-- To verify that our solution is correct we could check that all primes up to the summed number are actually used as input.

getPrimes :: [Integer] -> [Integer]
getPrimes [] = [] -- Base case is required as this will recursively call getPrimes until each number is either added as a prime, or removed from the list due to being divisible by smaller number
getPrimes (x:xs) = x : getPrimes [i | i <- xs, (i `mod` x) /= 0]

-- I started to try and make a list containing all consecutive lists (first with size 5). This implementation works... but if we use a finite list it will lead to an error when we get to the end of the list.
-- Eventually less than 5 elements will be in the list, and we will still take 5 numbers out of this.
findConsecutiveLists :: [Integer] -> Integer -> [[Integer]]
findConsecutiveLists t@(x:xs) n = (take 5 t) : findConsecutiveLists xs n -- TODO n instead of 5

-- Next 

findConsecutiveSums :: [[Integer]] -> [Integer]
findConsecutiveSums [] = []
findConsecutiveSums (x:xs) = (foldl (+) 0 x) : findConsecutiveSums xs

smallestPrimeNumberConsecutive :: Integer -> Integer
smallestPrimeNumberConsecutive n = head (filter (prime) (findConsecutiveSums (findConsecutiveLists (getPrimes [2..]) 5))) -- TODO Verify isPrime
