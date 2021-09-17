import Data.List

import Test.QuickCheck
import Data.List 

genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer)

generateList :: Integer -> [Integer]
generateList n = [1..n]

cardinality :: [a] -> Integer
cardinality [] = 0
cardinality (x:xs) = 1 + cardinality xs

factorial :: Integer -> Integer
factorial n = product [1..n]

testCardinalities :: Integer -> Bool
testCardinalities n = cardinality (permutations (generateList n)) == factorial n