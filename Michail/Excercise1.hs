module Lab1 where

import Test.QuickCheck
import Debug.Trace   
import System.Directory.Internal.Prelude (Int)
import GHC.Integer (Integer)
import Data.ByteString.Lazy (putStrLn)
import Data.List
import Data.List (subsequences)
import GHC.Base (Bool)

---- Excercise 1



-- Left side 

-- W1 Ex2
sumSquares :: Integer -> Integer
sumSquares n = sum[x^2 | x <- [1..n]]

-- W1 Ex3
sumCubes :: Integer -> Integer
sumCubes n = sum[x^3 | x <- [1..n]]

-- Right side
-- W1 Ex2
sumSquares' :: Integer -> Integer
sumSquares' n = n*(n+1)*(2*n+1) `div` 6

-- W1 Ex3
sumCubes' :: Integer -> Integer
sumCubes' n = (n*(n+1) `div` 2)^2

-- Applying induction 
-- Base case: Test if the first case is true

testBaseCase ::  (Integer -> Integer) -> (Integer -> Integer) -> Bool 
testBaseCase f g = f 1 == g 1


-- Although we cannot say that the equality of the two arguments works in every case we can run a significant amount of test to reach a fairly confident rerdict.
-- Method 1: Test the equality of the two arguments by generating multiple natural numbers and applying QuickCheck.

-- W1 Ex2
testSumSquares :: Integer -> Bool
testSumSquares n = let a = abs n in sumSquares a == sumSquares' a

-- W1 Ex3
testSumCubes :: Integer -> Bool
testSumCubes n = let a = abs n in sumCubes a == sumCubes' a

-- Generate natural numbers
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)



