module Exercise2 where
    
import Test.QuickCheck
import Lab1
import Data.List

---- EXERCISE 2 

-- Wk1Ex4
-- Prove by induction that if A is a finite set with |A| = n, then |P(A)| = 2^n

-- Mathematical Induction
-- Base Case
----  Empty set a has a cardinality n = 0 -> The powerset of an empty set by default has 1 elements.
---- It checks that P(0): 1 = 2 ^ 0 (True)
-- Induction Case
---- Inductive hypothesis
------ P(k) A set of k element has 2^k subsets
---- To be proved
------ A set B with k+1 elements has 2^(k+1) subsets
---- Proof 
------ B is a set with |B| = k+1
------ B = {e1,e2,..,ek,e(k+1)}
------ A is the original set: |A| = k and A has 2^k subsets
------ B = A U {e(k+1)}
------ Any subset of B either contains or not contains the element e(k+1)
------ The subsets of B that do not contain the element e(k+1) are substets of A as well
------ The last subsets mentioned, from the hypothesis, are 2^k
------ The subsets of B that contain e(k+1), are created by including the element in the 2^k subsets mentioned above
------ Therefore, B contains two categories of subsets, the 2^k ones not containing e(k+1) and the rest 2^k not containing the element
------ |P(B)|: 2^k+ 2^k = 2*2^k = 2^(k+1) (holds)

-- Haskell approach
-- Defined the set and a powerset to check the equality of |P(A)| = n^2 and generated multiple positive integers including 0. 
-- 
-- The algorithm is checking if the the given relation is true for the cardinalities of the two sets but not veryfing the equality for any given number.

--Creating a type Set
type Set a = [a]

-- The set
set :: Integer -> Set Integer
set n = [x | x <- [0..n-1]]

-- The powerset
powerset :: Integer -> Set (Set Integer)
powerset n = subsequences (set n)

{-
-- The quickcheck never reached 100 because elements grow exponentially which takes a lot of processing power.
testSets :: Integer -> Bool
testSets n = length (powerset n) == 2 ^ length (set n)
-}

-- testing with numbers from 0 to 9  
testSetsLim :: Integer -> Bool
testSetsLim n = let a = n`mod`9 in length (powerset a) == 2 ^ length (set a)

-- Generate natural numbers
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>=0)

-- Testing and printing
exercise2 :: IO ()
exercise2 = do
    putStrLn "--- Exercise 2 ---"
    putStrLn "--- Test W1EX4:"
    quickCheckResult $ forAll genPositiveIntegers testSetsLim
    putStrLn ""