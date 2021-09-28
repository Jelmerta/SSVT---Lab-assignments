module Bonus2 where

-- EULER PROBLEM 164
-- How many 20 digit numbers n (without any leading zero) exist such that no
-- three consecutive digits of n have a sum greater than 9?

-- APPROACH
-- We'll call a number "satisfactory" if it satisfies the problem criteria.
--
-- Keep track of a histogram that always has 100 bins: 00, 01, 02 ... 97, 98, 99.
-- The count in a bin, for example bin 23, represents the amount of satisfactory
-- m-digit numbers that end in 23.
--
-- The nice thing is that if we have calculated the histogram for a certain m,
-- we can calculate the histogram for m+1 from this, and so on, all the way to 20.
-- For example, the amount of satisfactory (m+1)-digit numbers ending in 23 is
-- equal to the amount of satisfactory m-digit numbers ending in 02, 12, 22,
-- 32 or 42 (but not 52 etc., because the sum of 523 would be greater than 9).
--
-- Finally when we have reached the histogram for 20-digit numbers, the answer
-- is just the sum of this histogram.
-- We use an iterative approach instead of recursive, because we need memoization.

indices :: [(Int,Int)]
indices = [(i,j) | i <- [0..9], j <- [0..9]]

initial :: [Int]
initial = map (\(i,j) -> fromEnum (i > 0 && i+j <= 9)) indices

targets :: (Int,Int) -> [(Int,Int)]
targets (i,j) = [(k,i) | k <- [0..(9-i-j)]]

calcSum :: [Int] -> [(Int,Int)] -> Int
calcSum xs targets = sum $ map (\(i,j) -> xs !! (10*i+j)) targets

f :: [Int] -> [Int]
f xs = map ((calcSum xs) . targets) indices


bonus2 :: IO ()
bonus2 = do
    putStrLn "\n--- Bonus problem 164 ---\n"
    putStrLn "How many 20 digit numbers n (without any leading zero) exist such that no three consecutive digits of n have a sum greater than 9?\n"

    print $ sum $ iterate f initial !! 18

    putStrLn "\nThe function could be tested by creating a bruteforce solution to the problem and then comparing the two functions for low input values."
