module Exercise3 where

import Test.QuickCheck
import Data.List
import MultiplicationTable
import Exercise2
import GHC.Integer (Integer)

-- https://wiki.haskell.org/99_questions/Solutions/28
lsort :: [[a]] -> [[a]]
lsort = sortBy (\xs ys -> compare (length xs) (length ys))



findMinimalPropSubsets :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Int
findMinimalPropSubsets p f = do
    x <- minimalPropSubsets (subsequences p) f
    return $ length x

minimalPropSubsets :: [[([Integer] -> Integer -> Bool)]] -> (Integer -> [Integer]) ->  Gen [[[Integer] -> Integer -> Bool]]
minimalPropSubsets [[]] _ = return []
minimalPropSubsets (subset:subsets) func = do
    x <- countSurvivors 4000 subset multiplicationTable
    m <- minimalPropSubsets subsets func
    return $ if x == 0 then subset : m else m


exercise3 :: IO ()
exercise3 = do
    r <- findMinimalPropSubsets properties multiplicationTable
    print r



