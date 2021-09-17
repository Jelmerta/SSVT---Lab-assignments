module Exercise3 where

import Data.List
import Test.QuickCheck

-- Excercise 3

-- WK1EX5

-- The number of possible permutations of given set of element is produced by the factorial of the cardinality of the set
-- Therefore, for a list xs with length n, the lentgh of the list containing all the permutations should be factorial n
-- To be proven: length(perms(list n)) == factorial(length(list n))

-- With this test, the mathematical fact is not proven for every list
-- However, we are testing that the factorial of the number of the elements of the list equals the number of permutations
-- of the same list. The length of the list is <= 0

-- Creating list
list :: Int -> [Int]
list n = [1..n]

-- Perms algorithm
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs))
    where
           insrt x [] = [[x]] 
           insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- factorial
factorial :: Int -> Int
factorial n = product [1..n]

-- factorial/perms with "big" natural numbers require a lot of processing power. Therefore the testing was limited
-- testing the equality with numbers 0-9
testPerms :: Int -> Bool
testPerms n = let a = n `mod` 9 in length (perms (list a)) == factorial(length (list a))

-- generate positive integers
genPositiveIntegers :: Gen Int
genPositiveIntegers = abs <$> (arbitrary :: Gen Int) `suchThat` (>0)

-- Testing and printing
exercise3 :: IO ()
exercise3 = do
    putStrLn "--- Exercise 3 ---"
    putStrLn "--- Test W1EX5:"
    quickCheckResult $ forAll genPositiveIntegers testPerms
    putStrLn ""