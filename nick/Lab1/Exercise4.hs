-- stack --install-ghc runghc

import Lab1
import Test.QuickCheck

-- All primes to number n this is the provided funcion by the lab with a little change.
primesToN :: Integer -> [Integer]
primesToN n = 2 : filter prime [3..n] 

allPrimeAndReversal :: [Integer]
allPrimeAndReversal = [i | i <- primesToN 10000, prime (reversal i)]

-- Check if all elements in the list are primes.
propAllPrimes :: Integer -> Bool
propAllPrimes n = length (filter prime allPrimeAndReversal) == length allPrimeAndReversal

-- Check if the reversals of all elements in the list are primes.
propAllReversalsPrimes :: Integer -> Bool
propAllReversalsPrimes n = length (filter prime [reversal i | i <- allPrimeAndReversal]) == length allPrimeAndReversal

-- Check if all elemnts in the list are < 10000.
propAllSmaller10000 :: Integer -> Bool
propAllSmaller10000 n = length (filter (> 10000) allPrimeAndReversal) == 0

main :: IO ()
main = do
    quickCheck (withMaxSuccess 1 propAllPrimes)
    quickCheck (withMaxSuccess 1 propAllReversalsPrimes)
    quickCheck (withMaxSuccess 1 propAllSmaller10000)