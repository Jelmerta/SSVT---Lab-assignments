-- stack --install-ghc runghc

import Lab1
import Test.QuickCheck

-- Take 101 primes starting from n.
primesFrom :: Integer -> [Integer]
primesFrom n = take 101 (filter (>= n) primes)

-- Check if the sum of a list is a prime.
sumIsPrime :: [Integer] -> Bool
sumIsPrime list = prime (sum (list))

consecutivePrimes :: Integer -> [[Integer]]
consecutivePrimes n = [[j | j <- primesFrom i] | i <- [1..]]

-- Does not work!!
correctPrimesList :: [Integer]
correctPrimesList = take 101 [y | i <- [1..], let x = primesFrom i, sumIsPrime x, y <- x]

correctPrime :: Integer
correctPrime = sum correctPrimesList

main :: IO ()
main = do
    print $ 