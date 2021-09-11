-- stack --install-ghc runghc

import Lab1
import Test.QuickCheck

nPrimes :: Int -> [Integer]
nPrimes n = take n primes

listProductPlusOne :: [Integer] -> Integer
listProductPlusOne list = product list + 1

-- Return values for n such that the list of n primes + 1 is not a prime
counterExamples :: [Int]
counterExamples = [i | i <- [1..], not (prime (listProductPlusOne (nPrimes i)))]

-- The smallest counterexample (value for n) is 6

main :: IO ()
main = do
    print counterExamples-- $ prime (listProductPlusOne (nPrimes 12))