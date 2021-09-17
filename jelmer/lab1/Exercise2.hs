import Test.QuickCheck
import Data.List

-- Exercise 4 from the workshop:
-- Prove by induction that if A is a finite set with |A| = n, then |Powerset(A)| = 2^n

-- This cannot be easily checked using QuickCheck as with large n the powerset of A will quickly become very large (2^n).
-- As this does not fit in memory, we can only check for small amounts of n that this is true

-- Another issue is that we have to check 

genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer)

generateList :: Integer -> [Integer]
generateList n = [1..n]

cardinality :: [a] -> Integer
cardinality [] = 0
cardinality (x:xs) = 1 + cardinality xs

cardinalityOfPowerSet :: [Integer] -> Integer
cardinalityOfPowerSet l = cardinality (subsequences l)

testCardinalities :: Integer -> Bool
testCardinalities n = 2 ^ cardinality (generateList n) == cardinalityOfPowerSet (generateList n)