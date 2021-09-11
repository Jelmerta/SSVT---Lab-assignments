module Exercise1 where

import Test.QuickCheck



---- EXERCISE 1

-- The equalities from the workshop will be tested using mathematical induction and then by quickcheck tests

-- W1EX2
-- Prove by induction that it holds for all natural numbers (n) that [ 1 + 2 + · · · + n = n(n+1)2]

-- Mathematical induction
-- Base Case (n = 1) (it is controversial if 0 is or is not a natural number)
---- P(0): 0 = 0 (True)
-- Inductive case 
---- Inductive Hypothesis (n = k)
------ P(k): 1+2+3+..+k = k(k+1)/2 -> means that 
---- To be proved
------ P(k+1): 1+2+3+..+(k+1) = (k+1)(k+2)/2
---- Proof
------ 1+2+3+..+k+(k+1) = k(k+1)/2+(k+1) = (k(k+1)+2(k+1))/2 = (k+2)(k+1)/2 (True)

-- In Haskell, it is not possible to implement mathematical induction to prove the equalities. 
-- However an approximate a conclusion can be reached by checking the equality of the two parts of the equation using multiple natural numbers
-- Thus, functions for each side of the equality were created and a generator to create random natural number to be tested


-- Left side 
sumSquares :: Integer -> Integer
sumSquares n = sum[x^2 | x <- [1..n]]

-- Right side
sumSquares' :: Integer -> Integer
sumSquares' n = n*(n+1)*(2*n+1) `div` 6

{- 
-- Created this function to test the base case but this is not required since we cannot follow with an inductive case
testBaseCase ::  (Integer -> Integer) -> (Integer -> Integer) -> Bool 
testBaseCase f g = f 1 == g 1
-}

-- Condition to be tested
testSumSquares :: Integer -> Bool
testSumSquares n = sumSquares n == sumSquares' n


-- W1EX3
-- Prove by induction that it holds for all natural numbers n that 1^2 + 2^2 + · · · + n^2 = n(n + 1)(2n + 1)/6 
-- Similarly to previous question 

-- Left Side
sumCubes :: Integer -> Integer
sumCubes n = sum[x^3 | x <- [1..n]]

-- Right Side
sumCubes' :: Integer -> Integer
sumCubes' n = (n*(n+1) `div` 2)^2

-- Condition to be tested
testSumCubes :: Integer -> Bool
testSumCubes n = sumCubes n == sumCubes' n

-- Generate natural numbers >0
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>=0)

-- Testing and printing
exercise1 :: IO ()
exercise1 = do
    putStrLn "--- Exercise 1 ---"
    putStrLn "--- Test W1EX2:"
    quickCheckResult $ forAll genPositiveIntegers testSumSquares
    putStrLn "--- Test W1EX3:"
    quickCheckResult $ forAll genPositiveIntegers testSumCubes
    putStrLn ""



