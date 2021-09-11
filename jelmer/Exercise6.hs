import Lab1
import Test.QuickCheck

genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

productsPlusOne :: [Integer] -> Integer
productsPlusOne xs = (foldl (*) 1 xs) + 1

-- -- Pseudocode-ish... not working yet I want to get the first elements of the product plus one where the result is not prime
-- -- I need a prime check function for this?
testConjecture :: Integer -> Bool
testConjecture n = prime(productsPlusOne (take (fromIntegral n) primes))

-- quickCheckResult $ forAll genPositiveIntegers testConjecture
-- Fails after testing with n = 6