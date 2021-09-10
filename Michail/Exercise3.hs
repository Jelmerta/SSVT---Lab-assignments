import Data.List
import Test.QuickCheck

-- Excercise 3

-- 

list :: Int -> [Int]
list n = [x | x <-[1..n]]

perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs))
    where
           insrt x [] = [[x]] 
           insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

factorial :: Int -> Int
factorial n = product [1..n]

testPerms :: Int -> Bool
testPerms n = length (perms (list n)) == factorial(length (list n))

genPositiveIntegers :: Gen Int
genPositiveIntegers = abs <$> (arbitrary :: Gen Int) `suchThat` (>0)
