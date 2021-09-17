module Exercise1 where

import System.Random
import Data.List
import Data.Array.IO.Internals (IOArray)
import System.IO (print)
import Data.Bool (Bool)

-- Your programmer Red Curry has written the following function for generating lists of floating point numbers.

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
    p <- getStdRandom random
    ps <- probs (n-1)
    return (p:ps)

-- He claims that these numbers are random in the open interval (0..1). 
-- Your task is to test whether this claim is correct, by counting the numbers in the quartiles



-- We need to create a function to test the postcondition
-- The preconditions its that the input is a positive integer (including 0)
-- The postcondition is that the output is a (fairly) random Float f where fE(0,1)
-- Although absolut randomness cannot be achieved with any known tool
-- We assume that it is random if the output has the same probability to be any number in the given domain
-- To check this, we are dividing the output domain in quartiles in order to check: 
-- if the probability of an output belonging in every quartile of the domain is virtually the same


-- Checks if a value belongs in a range
checkQuartile :: Float -> Float -> Float -> Bool
checkQuartile x l u = (x >= l) && (x < u)

-- Test if all the numbers are in the given range
test1 :: [Float] -> Bool
test1 [] =  True
test1 (x:xs) = (x > 0 && x < 1) && test1 xs

-- Counts the number of values in each quartile
test2 :: [Float] -> [Int]
test2 [] = [0,0,0,0]
test2 (x:xs) | checkQuartile x 0.00001 0.25 = zipWith (+) [1,0,0,0] (test2 xs)
             | checkQuartile x 0.25 0.5 = zipWith (+) [0,1,0,0] (test2 xs)
             | checkQuartile x 0.5 0.75 = zipWith (+) [0,0,1,0] (test2 xs)
             | checkQuartile x 0.75 1.0 = zipWith (+) [0,0,0,1] (test2 xs)





exercise1 :: IO ()
exercise1 = do
    putStrLn "\n--- Exercise 1 ---"
    putStrLn "\nTest 1: check if all the elements are in the given range\n"
    t1 <-  test1 <$> probs 10000 
    print t1
    putStrLn "\nTest 2: check how many elements of the output list belong to every quartile\n"
    t2 <- test2 <$> probs 10000
    print t2
    putStrLn "\nThe results, as expected, show that any number of the list has virtually"
    putStrLn "the same probability of belonging in any of the quartiles.\n"
    putStrLn "However, this is not a proof that the function produces only random numbers.\n"
    putStrLn "Counterexample: if the output of probs 9 was the list\n[0.1,0.2,0.3,0.5,0.6,0.7,0.8,0.9] the test would succeed,\nbut the output would indicate that the sequence is not random.\n"
    putStrLn "The last example could be translated in a strong postcondition,\nthat could be tested. However, It would not eliminate the\ncertainty that the sequence of the elements has an underlying pattern.\n"
    putStrLn ""










