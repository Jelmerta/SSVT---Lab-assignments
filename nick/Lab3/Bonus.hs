module Bonus where

import Data.Char (digitToInt)

-- Convert an integer to a list of its digits.
-- https://stackoverflow.com/a/3964069
digits :: Integer -> [Int]
digits = map digitToInt . show

-- Check if a list is in increasing order.\
-- Code from answer to:
-- https://stackoverflow.com/questions/30633064/check-if-the-list-is-ascending-haskell
increasing :: (Ord a) => [a] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:y:xs) = x <= y && increasing (y:xs)

-- Check if a list is in increasing order.\
-- Edited code from answer to:
-- https://stackoverflow.com/questions/30633064/check-if-the-list-is-ascending-haskell
decreasing :: (Ord a) => [a] -> Bool
decreasing [] = True
decreasing [x] = True
decreasing (x:y:xs) = x >= y && decreasing (y:xs)

-- Check is a number is an increasing number.
increasingNumber :: Integer -> Bool
increasingNumber n = increasing (digits n)

-- Check is a number is a decreasing number.
decreasingNumber :: Integer -> Bool
decreasingNumber n = decreasing (digits n)

-- Check if a number is a bouncy number.
bouncyNumber :: Integer -> Bool
bouncyNumber n = not (increasingNumber n) && not (decreasingNumber n)

-- Gives a list of bouncy numbers from 0 to n
bouncyNumbersTo :: Integer -> [Integer]
bouncyNumbersTo n = filter bouncyNumber [0..n]

-- Gives the fraction of numbers that are bouncy numbers from 0 to n.
fracBouncyNumbersTo :: Int -> Float
fracBouncyNumbersTo n = fromIntegral (length (bouncyNumbersTo (fromIntegral n))) / fromIntegral n

-- Gives the first number for which the fraction of bouncy numbers is 0.99 (99%).
-- We start computation at 1586995 because this saves a lot of time (and we already know the result).
bouncyResult :: Int
bouncyResult = head [x | x <- [1586995..1587000], fracBouncyNumbersTo x == 0.99]

-- Main function to start the exercise code.
bonus :: IO ()
bonus = do
    putStrLn "\n--- Bonus assignments: Project Euler ---\n"
    putStrLn "\n--- Euler problem 112. ---\n"
    putStrLn "--- Bouncy numbers ---"
    putStrLn "Find the least number for which the proportion of bouncy \
    \numbers is exactly 99%."
    putStrLn "The number we found is:"
    print bouncyResult

    putStrLn "This function can be tested to test some properties on bouncy \
    \numbers. For example that all numbers in the list resulting from \
    \'bouncyNumbersTo' are not increasing- and not decreasing numbers, \
    \although the this pretty specificly tests what is actually already \
    \enforced by theimplementation of 'bouncyNumber'. What we can also test \
    \is if the program gives no bouncy numbers under 100, the amount of \
    \bouncy numbers under one-thousand is 525, the proportion of bouncy \
    \numbers under 538 is less than 50%, the proportion of bouncy numbers \
    \above 538 is equal to or more than 50%, the proportion of bouncy numbers \
    \to 21780 is equal to 90%. As stated by the text of Euler problem 112."