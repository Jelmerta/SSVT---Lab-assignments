module Exercise2 where

import SetOrd
import Data.List
import Exercise1
import Test.QuickCheck
import GHC.Types (Bool)

-- Author: Michail Skalistiris Pikis
set1 :: Set Int
set1 = Set[1,2,3,4,5]
set2 :: Set Int
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

setEquality :: Ord a => Set a -> Set a -> Bool
setEquality setA setB = subSet setA setB && subSet setB setA

---- Postconditions

------ Property 1:  A Π Empty Set = Empty Set
testEmptySetIntersection :: Ord a => Set a -> Bool
testEmptySetIntersection setA = (intersectionSet setA (Set [])) == (Set [])

------ Property 2: Idempotence A Π Α = Α
testIdepotenceIntersection :: Ord a => Set a -> Bool
testIdepotenceIntersection setA = (intersectionSet setA setA) == setA

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


{-}
testIntersectionSet :: Int -> IO (Bool)
testIntersectionSet 0 = True
testIntersectionSet (-1) = False
testIntersectionSet n = do 
-}




-- UNION
-- 2. A U B = { x | (x E A) V (x E B)}

-- unionSet from SetOrd

-- DIFFERENCE
-- 3. A - B = {x | (x E A) Λ (x notE B)}
differenceSet ::(Ord a) => Set a -> Set a -> Set a
differenceSet (Set []) _ = Set []
differenceSet (Set (x:xs)) (Set ys) | inSet x (Set ys) = differenceSet (Set xs) (Set ys)
                                      | otherwise = insertSet x (differenceSet (Set xs) (Set ys))



exercise2 :: IO ()
exercise2 = do
    quickCheck $ forAll quickCheckRandSet testEmptySetIntersection
    quickCheck $ forAll quickCheckRandSet testIdepotenceIntersection
    quickCheck $ forAll quickCheckRandSet2 (uncurry testCommutativityIntersection)
    quickCheck $ forAll quickCheckRandSet3 (uncurry3 testAssociativityIntersection)
    quickCheck $ forAll quickCheckRandSet3 (uncurry3 testDistributivityIntersectioni)
    quickCheck $ forAll quickCheckRandSet3 (uncurry3 testDistributivityIntersectionii)





