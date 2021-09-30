module Exercise2 where

import SetOrd
import Data.List
import Exercise1
import Test.QuickCheck
import GHC.Types (Bool)
import GHC.Base (Bool)
import SetOrd (emptySet)

-- Author: Michail Skalistiris Pikis
-- 5 hours

set1 = Set[1,2,3,4,5]
set2 = Set[4,5,6,7]


uncurry3 :: (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3 f = \(a, b, c) -> f a b c

-- SETS A, B

-- INTERSECTION
-- 1. A Π B = { x | (x Ε A) Λ (x Ε B) } 
---- Preconditions
------ Valid Sets

intersectionSet ::(Ord a) => Set a -> Set a -> Set a
intersectionSet (Set []) _ = Set []
intersectionSet  _ (Set []) = Set []
intersectionSet (Set (x:xs)) (Set ys) | inSet x (Set ys) = insertSet x (intersectionSet (Set xs) (Set ys))
                                      | otherwise = intersectionSet (Set xs) (Set ys)

---- Postconditions

------ Property 1:  A Π Empty Set = Empty Set
testEmptySetIntersection :: Ord a => Set a -> Bool
testEmptySetIntersection setA = (intersectionSet setA (Set [])) == (Set [])

------ Property 2: Idempotence A Π Α = Α
testIdempotenceIntersection :: Ord a => Set a -> Bool
testIdempotenceIntersection setA = (intersectionSet setA setA) == setA

------ Property 3: Commutativity A Π Β = Β Π Α
testCommutativityIntersection :: Ord a => Set a -> Set a -> Bool
testCommutativityIntersection setA setB = (intersectionSet setA setB) == (intersectionSet setB setA)

------ Property 4: Associativity A Π (Β Π C) = (A Π Β) Π C
testAssociativityIntersection :: Ord a => Set a -> Set a -> Set a -> Bool
testAssociativityIntersection setA setB setC = (intersectionSet setA (intersectionSet setB setC)) == (intersectionSet (intersectionSet setB setA) setC)

------ Property 5: Distributivity 
-------- i: A Π (Β U C) = (A Π Β) U (A Π C)
-------- ii: A U (Β Π C) = (A U Β) Π (A U C)
testDistributivityIntersectioni,testDistributivityIntersectionii  :: Ord a => Set a -> Set a -> Set a -> Bool
testDistributivityIntersectioni setA setB setC = (intersectionSet setA (unionSet setB setC)) == (unionSet (intersectionSet setA setB) (intersectionSet setA setC))
testDistributivityIntersectionii setA setB setC = (unionSet setA (intersectionSet setB setC)) == (intersectionSet (unionSet setA setB) (unionSet setA setC))






-- UNION
-- 2. A U B = { x | (x E A) V (x E B)}

-- unionSet from SetOrd

---- Postconditions

------ Property 1:  A Π Empty Set = A
testEmptySetUnion :: Ord a => Set a -> Bool
testEmptySetUnion setA = (unionSet setA (Set [])) == setA

------ Property 2: Idempotence A U Α = Α
testIdempotenceUnion :: Ord a => Set a -> Bool
testIdempotenceUnion setA = (unionSet setA setA) == setA

------ Property 3: Commutativity A U Β = Β U Α
testCommutativityUnion :: Ord a => Set a -> Set a -> Bool
testCommutativityUnion setA setB = (unionSet setA setB) == (unionSet setB setA)

------ Property 4: Associativity A U (Β U C) = (A U Β) U C
testAssociativityUnion :: Ord a => Set a -> Set a -> Set a -> Bool
testAssociativityUnion setA setB setC = (unionSet setA (unionSet setB setC)) == (unionSet (unionSet setB setA) setC)

------ Property 5: Distributivity (Already Defined for Intersection)
-------- i: A Π (Β U C) = (A Π Β) U (A Π C)
-------- ii: A U (Β Π C) = (A U Β) Π (A U C)




-- DIFFERENCE
-- 3. A - B = {x | (x E A) Λ (x notE B)}
differenceSet ::(Ord a) => Set a -> Set a -> Set a
differenceSet (Set []) _ = Set []
differenceSet (Set (x:xs)) (Set ys) | inSet x (Set ys) = differenceSet (Set xs) (Set ys)
                                      | otherwise = insertSet x (differenceSet (Set xs) (Set ys))

-- Postconditions:
---- Property 1: The the diference of the and its self should be an empty set 
---- A - A = {}
testSelfDifference :: Ord a => Set a -> Bool
testSelfDifference setA = differenceSet setA setA == emptySet

---- Property 2: The elements of the output should be included only in the first set 
--testInclusionDifference :: Ord a => Set a -> Set a -> Bool
-- testInclusionDifference setA setB = (all (differenceSet setA setB) setA) && not (any (differenceSet setA setB) setB)


-- Custom testing

-- For functions with one set
testingSet1 :: (Set Int -> Bool)-> Int -> IO ()
testingSet1 f n| n == 0 = print True
                              | otherwise = do 
                                  a <- genRandSet
                                  if (f a) then (testingSet1 f (n-1)) else print False


-- For functions with two sets
testingSet2 :: (Set Int -> Set Int -> Bool)-> Int -> IO ()
testingSet2 f n| n == 0 = print True
                              | otherwise = do 
                                  a <- genRandSet
                                  b <- genRandSet
                                  if (f a b) then (testingSet2 f (n-1)) else print False

-- For functions with three sets
testingSet3 :: (Set Int -> Set Int -> Set Int -> Bool)-> Int -> IO ()
testingSet3 f n| n == 0 = print True
                              | otherwise = do 
                                  a <- genRandSet
                                  b <- genRandSet
                                  c <- genRandSet
                                  if (f a b c) then (testingSet3 f (n-1)) else print False




exercise2 :: IO ()
exercise2 = do
    putStrLn "\n--- Exercise 2 ---"
    putStrLn "\n-- INTERSECTIONS"
    putStrLn "\n- Custom Tests\n"
    putStrLn "Test 1: with empty Set\n"
    testingSet1 testEmptySetIntersection 100
    putStrLn "\nTest 2: Idempotence\n"
    testingSet1 testIdempotenceIntersection 100
    putStrLn "\nTest 3: Commutativity\n"
    testingSet2 testCommutativityIntersection 100
    putStrLn "\nTest 4: Associativity\n"
    testingSet3 testAssociativityIntersection 100
    putStrLn "\nTest 5: Distributivity i\n"
    testingSet3 testDistributivityIntersectioni 100
    putStrLn "\nTest 6: Distributivity ii\n"
    testingSet3 testDistributivityIntersectionii 100
    putStrLn "\n- QuickCheck Tests: In the same order\n"
    quickCheck $ forAll quickCheckRandSet testEmptySetIntersection
    quickCheck $ forAll quickCheckRandSet testIdempotenceIntersection
    quickCheck $ forAll quickCheckRandSet2 (uncurry testCommutativityIntersection)
    quickCheck $ forAll quickCheckRandSet3 (uncurry3 testAssociativityIntersection)
    quickCheck $ forAll quickCheckRandSet3 (uncurry3 testDistributivityIntersectioni)
    quickCheck $ forAll quickCheckRandSet3 (uncurry3 testDistributivityIntersectionii)
    putStrLn "\n-- UNIONS: Distributivity has been checked already"
    putStrLn "\n- Custom Tests\n"
    putStrLn "Test 1: with empty Set\n"
    testingSet1 testEmptySetUnion 100
    putStrLn "\nTest 2: Idempotence\n"
    testingSet1 testIdempotenceUnion 100
    putStrLn "\nTest 3: Commutativity\n"
    testingSet2 testCommutativityUnion 100
    putStrLn "\nTest 4: Associativity\n"
    testingSet3 testAssociativityUnion 100
    putStrLn "\n- QuickCheck Tests: In the same order\n"
    quickCheck $ forAll quickCheckRandSet testEmptySetUnion
    quickCheck $ forAll quickCheckRandSet testIdempotenceUnion
    quickCheck $ forAll quickCheckRandSet2 (uncurry testCommutativityUnion)
    quickCheck $ forAll quickCheckRandSet3 (uncurry3 testAssociativityUnion)
    putStrLn "\n-- DIFFERENCE\n"
    putStrLn "- Custom Tests\n"
    putStrLn "Test 1: A - A = {}\n"
    testingSet1 testSelfDifference 100
    putStrLn "\nAnother property considered in included in the comments"
    putStrLn "\n- QuickCheck Tests\n"
    quickCheck $ forAll quickCheckRandSet testSelfDifference
    putStrLn ""







