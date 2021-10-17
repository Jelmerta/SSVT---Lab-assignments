module Exercise3 where

import Test.QuickCheck
import Data.List
import MultiplicationTable
import Exercise2

-- 5 hours

-- Exercise 3: Implement a function that calculates the minimal property subsets, 
-- given a 'function under test' and a set of properties.

-- To solve this I will create a function that checks all the subsets (subsequences) of the sets of properties
-- and finds which of the subsets has no survivors
-- All the subsets that result to 0 survivors with the lowest number of properties possible are the minimal property subsets

-- Check for every subset if it is a complete specification and return those subsets
completeSpecSubsets :: [[([Integer] -> Integer -> Bool)]] -> (Integer -> [Integer]) ->  Gen [[[Integer] -> Integer -> Bool]]
completeSpecSubsets [[]] _ = return []
completeSpecSubsets (subset:subsets) func = do
    x <- countSurvivors 4000 subset multiplicationTable
    m <- completeSpecSubsets subsets func
    return $ if x == 0 then subset : m else m

-- sort subsets by length
-- https://wiki.haskell.org/99_questions/Solutions/28
lsort :: [[a]] -> [[a]]
lsort = sortBy (\xs ys -> compare (length xs) (length ys))

-- get the subsets with the minimum number of properties for an ordered by length list of lists
minimal :: [[a]] -> [[a]]
minimal [[]] = [[]]
minimal (xs:ys:xss) | length xs == length ys = xs : minimal(ys : xss)
                           | otherwise = [[]]

-- Input a list of testing properties and a function and get the minial properties subset for the list
minimalPropSubsets :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen [[([Integer] -> Integer -> Bool)]]
minimalPropSubsets p f = do
    x <- completeSpecSubsets (subsequences p) f
    return $ minimal (lsort x)




exercise3 :: IO ()
exercise3 = do
    putStrLn "-- Exercise 3 --\n"
    putStrLn "Most of the solution has been implemented but some features are missing"

    



