-- Time spent: 4 hours

module Exercise4 where

import Data.List
import Test.QuickCheck
import SetOrd
import Control.Monad

type Rel a = [(a, a)]

-- A relation R is serial on a domain A if for any x ∈ A there is an y ∈ A such that xRy.

-- 1. Write a function for checking wheter a relation is serial
-- *Exercise4> isSerial [1] [(1,3),(2,4)]
-- True
-- *Exercise4> isSerial [2] [(1,3),(2,4)]
-- True
-- *Exercise4> isSerial [1,2] [(1,3),(2,4)]
-- True
-- *Exercise4> isSerial [1,2,3] [(1,3),(2,4)]
-- False
-- *Exercise4> isSerial [1,2,3] [(1,3),(2,4),(3,5)]
-- True
-- *Exercise4> isSerial [1,2,3] []
-- False
-- *Exercise4> isSerial [] [(1,3),(2,4),(3,5)]
-- True
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial [] r = True -- Empty input list is always contained in any relation
isSerial [d] r = [1 | (x,y) <- r, x == d] /= [] -- Single element can be checked by looping over the relation and finding if there is any input matching
isSerial (d:domain) r = isSerial [d] r && isSerial domain r

-- 2. Test your implementation with two QuickCheck properties.
-- The length of the relation should be larger or equal to the domain, as everything in the domain fits in the relation.
prop_serial_length_check :: Rel Integer -> Property
prop_serial_length_check rel = isSerial (domR rel) rel ==> (length (domR rel)) <= length rel

--https://en.wikipedia.org/wiki/Serial_relation states that a reflexive relation is a serial relation, which we can check
-- *Exercise4> isReflexive [1,2] [(1,1),(2,2),(2,1),(1,2)]
-- True
-- *Exercise4> isReflexive [1,2] [(2,1),(1,2)]
-- False
-- *Exercise4> isReflexive [1,2] [(1,1),(2,1),(1,2)]
-- False
-- *Exercise4> isReflexive [] [(1,1),(2,1),(1,2)]
-- True
-- *Exercise4> isReflexive [1,2] []
-- False
isReflexive :: Eq a => [a] -> Rel a -> Bool
isReflexive domain rel = length domain == length [1 | (x,y) <- rel, x == y, x `elem` domain, (x,x) `elem` rel]


-- We found out that you can find the domain of a relation as defined in Haskell Road 5.3.
-- domR simply finds out all the domain values in the pairs and adds them together.
domR :: Rel Integer -> [Integer]
domR r = [d | (d,_) <- r]


prop_serial_reflex_is_serial :: Rel Integer -> Property
prop_serial_reflex_is_serial rel = isReflexive (domR rel) rel ==> isSerial (domR rel) rel

-- An attempt was made to test using a generator, but we were unable to finish this in time.
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

genRelationPair :: Gen (Integer, Integer)
genRelationPair = liftM2 (,) genPositiveIntegers genPositiveIntegers

-- genRelation :: Gen (Rel Integer)
-- genRelation = 

-- We can use reflexive relations as input for our serial relations, as all reflexive relations are serial. This might leave out some other relations that may be relevant.
-- genReflexiveRelation :: Int -> Gen (Rel Int)
-- genReflexiveRelation = 

-- We wanted to make serial relation by selection a domain from 0 to n and then creating an image with values that are in the domain.
-- genSerialRelation :: Integer -> Gen (Rel Integer)
-- genSerialRelation  n = do
    -- domain <- [0..n] --vectorOf n (choose (0,n))
    -- image <- vectorOf n (choose (0,n))
    -- return (zip domain image)


----------------------------------------------------------------------------------
-- Question 3. Consider the relation R = {(x, y) | x = y(mod n)}, where (mod n) is the modulo function in modular arithmetic and n > 0. Discuss whether (and when) R is serial. How can you test whether R is serial? How can you prove that R is serial?
-- Relation R uses a mod operator that looks for equivalence between x and y. For example 38 = 26 (mod 12), as mod 12 applied to 38 = 2, and mod 12 applied to 26 = 2, so this is a valid solution.)
-- mod n has solutions for x and y when x = (n*k) + b and y = (n*l) + b, k and l may be different factors.
-- To find out when R is serial we need to make sure that all elements in the domain fit this formula.
-- Wikipedia mentions that this operator is a congruence relation: https://en.wikipedia.org/wiki/Congruence_relation(which means the elements are grouped into equivalence classes), which is an equivalence relation
-- An equivalence relation is by definition reflexive.
-- A reflexive relation is by definition serial. This concludes that relation R is serial.

-- Intuitively: 
-- Multiple relations may exist for every n: with n = 2, we have the solution {0,2,4,6,...} as well as {1,3,5,7,...}
-- In fact: it seems like for every n, there are n sets of solutions: for n=3, we get {0,3,6,...}, {1,4,7,...}, {2,5,8,...}
-- Another possibility is simply the identity set: (0,0), (1,1) etc. are also valid solutions.
-- R is therefore always serial: it is always possible to find (x,x) for every x in the domain. 

-- Found out if is the relation is serial by checking uniqueness of applying the mod function on every element in the domain.
testModSeriality :: Integer -> [Integer] -> Bool
testModSeriality n [] = True
testModSeriality n domain = length (nub [x `mod` n | x <- domain]) == 1

-- An attempt was made to create a property to verify this is the case.
-- Assuming x != y, as otherwise the problem is not very interesting, as we can always find the same element again
-- prop_modSeriality :: [Integer] -> Integer -> Bool
-- prop_modSeriality [] n = True
-- prop_modSeriality (d:domain) n 
--     | (mod n d) `elem` (map (mod n) domain) = prop_modSeriality (filter (\ x -> (mod n x) /= (mod n d)) domain) n

-- We can arbitrarily generate x,y that are included in the relation, and check that every x and y there is a relation with x and y in the domain
genModNIntegers :: Integer -> Integer -> Gen Integer
genModNIntegers n o = (+o) <$> ((*n) <$> (arbitrary :: Gen Integer))

exercise4 :: IO ()
exercise4 = do
    putStrLn "\n--- Exercise 4---\n\n"
    putStrLn "In this exercise, we will write a function that checks whether a relation R is serial."
    putStrLn "For a relation to be serial on a domain A,"
    putStrLn "it has to be true that for any x ∈ A there is a y ∈ A such xRy"

    quickCheck prop_serial_length_check
    -- Not disproven, though a generator may help with finding more relevant tests:
    -- *** Gave up! Passed only 48 tests; 1000 discarded tests.
    quickCheck prop_serial_reflex_is_serial

    -- quickCheck $ forAll genPositiveIntegers $ \n -> prop_modSeriality domain n
    -- verboseCheck $ forAll formGen $ \input -> isCnf (cnf1' input)