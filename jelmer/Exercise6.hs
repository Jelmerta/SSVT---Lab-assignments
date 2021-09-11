getPrimes :: [Integer] -> [Integer]
getPrimes [] = [] -- Base case is required as this will recursively call getPrimes until each number is either added as a prime, or removed from the list due to being divisible by smaller number
getPrimes (x:xs) = x : getPrimes [i | i <- xs, (i `mod` x) /= 0]

productsPlusOne :: [Integer] -> Integer
productsPlusOne xs = foldl (*) xs + 1

-- Pseudocode-ish... not working yet I want to get the first elements of the product plus one where the result is not prime
-- I need a prime check function for this?
testConjecture :: Integer
testConjecture = head $ filter (productsPlusOne (getPrimes [2..]) /= prime)