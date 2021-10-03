module Exercise4 where

import Data.List
import Test.QuickCheck
import SetOrd

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
prop_serial_length_check :: [Integer] -> Rel Integer -> Property
prop_serial_length_check domain rel = isSerial domain rel ==> (length domain <= length rel)

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
-- isReflexive rel = (length domain) == (length (filter (\ d -> (fst d, fst d) `elem` rel) rel))  --[1 | (x,y) <- rel, x == y, x ==]

prop_serial_reflex_is_serial :: [Integer] -> Rel Integer -> Property
prop_serial_reflex_is_serial domain rel = isReflexive domain rel ==> isSerial domain rel

-- Consider the relation R = {(x, y) | x = y(mod n)}, where (mod n) is the modulo function in modular arithmetic and n > 0. Discuss whether (and when) R is serial. How can you test whether R is serial? How can you prove that R is serial?
-- Relation R uses a mod operator that looks for equivalence between x and y. For example 38 = 26 (mod 12), as mod 12 applied to 38 = 2, and mod 12 applied to 26 = 2, so this is a valid solution.)
-- mod n has solutions for x and y when x = (n*k) + b and y = (n*l) + b, k and l may be different factors.
-- To find out when R is serial we need to make sure that all elements in the domain fit this formula.

-- Found out if is the relation is serial by checking uniqueness of applying the mod function every element in the domain.
testModSeriality :: Integer -> [Integer] -> Bool
testModSeriality n [] = True
testModSeriality n domain = length (nub [x `mod` n | x <- domain]) == 1

-- Assuming x != y, as otherwise the problem is not very interesting, as we can always find the same element again
-- prop_modSeriality :: [Integer] -> Integer -> Bool
-- prop_modSeriality [] n = True
-- prop_modSeriality (d:domain) n 
--     | (mod n d) `elem` (map (mod n) domain) = prop_modSeriality (filter (\ x -> (mod n x) /= (mod n d)) domain) n

-- prop_modSeriality :: Integer -> Rel Integer -> Boolean
-- prop_modSeriality [] n = True
-- prop_modSeriality n ((x,y):rel)
--     | mod n x == mod n y =

genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

genModNIntegers :: Integer -> Integer -> Gen Integer
genModNIntegers n o = (+o) <$> ((*n) <$> (arbitrary :: Gen Integer))

-- We can arbitrarily generate x,y that are included in the relation, and check that every x and y there is a relation with x and y in the domain

-- Multiple relations may exist for every n: with n = 2, we have the solution {0,2,4,6,...} as well as {1,3,5,7,...}
-- for every n, there seems to n relations R that are all valid.
-- Another possible solution may be (0,2) and (1,3), which has a solution for every element in the set (2,0) and (3,1) are also correct.
-- Another possibility is simply the identity set: (0,0), (1,1) etc. are also valid solutions.
-- R is therefore always serial: it is always possible to find (x,x) for every x in the domain. TODO Is this really how it works... probably not?

-- To prove that R is serial: We know that there is always a y with solution y = n

exercise4 :: IO ()
exercise4 = do
    putStrLn "\n--- Exercise 4---\n\n"
    putStrLn "In this exercise, we will write a function that checks whether a relation R is serial."
    putStrLn "For a relation to be serial on a domain A,"
    putStrLn "it has to be true that for any x ∈ A there is a y ∈ A such xRy"

    -- Not disproven, though a generator may help with finding more relevant tests:
    -- *** Gave up! Passed only 51 tests; 1000 discarded tests. 
    quickCheck prop_serial_length_check
    -- *** Gave up! Passed only 48 tests; 1000 discarded tests.
    quickCheck prop_serial_reflex_is_serial

    -- quickCheck $ forAll genPositiveIntegers $ \n -> prop_modSeriality domain n
    -- verboseCheck $ forAll formGen $ \input -> isCnf (cnf1' input)