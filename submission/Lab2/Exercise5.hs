-- Time spent: 4 hours

-- Recognizing and generating derangements

module Exercise5 where

import Test.QuickCheck
import Data.List

-- Derangements are defined as all of the permutations of a list of elements, such that
-- no element in the permutation remains in the same index.
-- For example: For the list of elements [1,2,3], all of the permutations would be:
-- [[1,2,3],[2,1,3],[3,2,1],[2,3,1],[3,1,2],[1,3,2]]
-- but the permutation [2,1,3] for example, would have the third element in the same index, and is therefore not a derangement.
-- The derangements of [1,2,3] would be:
-- [2,3,1],[3,1,2]
-- To find out the derangements of a list, we can calculate all of the permutations of a list and check for every permutation if none of the elements in the same index are the same

-- We can reuse the isPermutation check from ex4 to check for derangements
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] a = null a
isPermutation (h:t) a = isPermutation t (filter (/= h) a)

-- To check if an input list is a derangement of a target list we verify that for every index in the list, we find a different element.
-- To find out if none of the indices match, we sum the amount of times x != y for the same index, and match it with the length of one of the inputs
-- This assumes that xs and ys have the same size
isDerangement :: (Eq a) => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && ((sum [fromEnum (x /= y) | (x, y) <- zip xs ys]) == length xs)

-- To find out the derangements of a list, we can find out the permutations and simply filter out
-- all permutations that do not match the a derangement according to the function above
deran :: Integer -> [[Integer]]
deran n = filter (isDerangement [1..n-1]) (permutations [1..n-1])


-- Testing isDerangement:

-- Using ==> instead of --> for testing to make sure QuickCheck performs 100 checks for cases where the premise is true
-- Property 1: The lengths of both input and target list need to be equal for them to be derangements
testIsDerangementLengthProperty :: [Integer] -> [Integer] -> Bool
testIsDerangementLengthProperty xs ys = length xs == length ys

prop_derangementLength :: [Integer] -> [Integer] -> Property
prop_derangementLength xs ys = isDerangement xs ys ==> testIsDerangementLengthProperty xs ys

-- Property 2: If list a is a derangement of list b, neither will have duplicate elements
testIsDerangementNoDuplicates :: [Integer] -> [Integer] -> Bool
testIsDerangementNoDuplicates xs ys = (length (nub xs) == length xs) && (length (nub ys) == length ys)

prop_derangementNoDuplicates :: [Integer] -> [Integer] -> Property
prop_derangementNoDuplicates xs ys = isDerangement xs ys ==> testIsDerangementNoDuplicates xs ys

-- Property 3: Elements in the derangements are also in the original
testIsDerangementContainsSameElements :: [Integer] -> [Integer] -> Bool
testIsDerangementContainsSameElements xs ys = (sum [fromEnum(x `elem` ys) | x <- xs]) == length xs

prop_derangementContainsSameElements :: [Integer] -> [Integer] -> Property
prop_derangementContainsSameElements xs ys = isDerangement xs ys ==> testIsDerangementContainsSameElements xs ys

-- Property 4: Derangements are permutations of each other
testIsDerangementPermutation :: [Integer] -> [Integer] -> Bool
testIsDerangementPermutation = isPermutation

prop_derangementPermutation :: [Integer] -> [Integer] -> Property
prop_derangementPermutation xs ys = isDerangement xs ys ==> testIsDerangementPermutation xs ys

-- We define a well-chosen domain for testing, some of which would contradict these properties.
-- All combinations of the inputs as input and target lists can be generated.
-- []       []      True
-- [1]      []      False
-- []       [1]     False
-- [1]      [1]     False
-- [1,2]    [1]     False
-- [1]      [1,2]   False
-- [1,2]    [1,2]   False
-- [1,2]    [2,1]   True
-- [2,1]    [1,2]   True
-- [1,2]    [2,3]   False
-- [2,3]    [1,2]   False

inputs :: [[Integer]]
inputs = [[], [1], [1,2], [2,1], [2,3]]

randomInput :: Gen [Integer]
randomInput = elements inputs

-- To compare the strengths, we reuse the code from the lab/ex3, but with some variations to work with functions with two list inputs.
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Provided functions for testing property strength
stronger, weaker :: [a] -> [a] -> (a -> a -> Bool) -> (a -> a -> Bool) -> Bool
stronger xs ys p q = forall xs (\ x -> forall ys (\ y -> p x y --> q x y))
weaker   xs ys p q = stronger xs ys q p

-- To compare properties, we can make use of the following code, which will tell us if a property is stronger, weaker or equivalent
compar :: [a] -> [a] -> (a -> a -> Bool) -> (a -> a -> Bool) -> String
compar xs ys p q = let pq = stronger xs ys p q
                       qp = stronger xs ys q p
                in
                    if pq && qp then "equivalent"
                    else if pq  then "stronger"
                    else if qp  then "weaker"
                    else             "incomparable"

propertySort :: String -> String -> Ordering
propertySort prop1 prop2
    | compar [[], [1], [1,2], [2,1], [2,3]] [[], [1], [1,2], [2,1], [2,3]] (findPropFromName prop1) (findPropFromName prop2) == "stronger" = LT
    | otherwise = GT
    -- If compar of two properties is equal, it does not matter if GT or LT is reached, as the property will be on one side of the other property.
    -- This does not account for incomparable. It would probably be better if the property would not be added to the sorting, or perhaps just fail completely.
    -- This does not matter in this exercise as the properties are comparable here.

-- Sadly we can't simply print function names with type (Integer -> Bool), so we have to be clever and first find the right function to be able to sort the properties
-- To make sure we order on the property, this also has the effect of executing the function here
findPropFromName :: String -> ([Integer] -> [Integer] -> Bool)
findPropFromName property
    | property == "testIsDerangementLengthProperty" = testIsDerangementLengthProperty
    | property == "testIsDerangementNoDuplicates" = testIsDerangementNoDuplicates

    | property == "testIsDerangementContainsSameElements" = testIsDerangementContainsSameElements
    | property == "testIsDerangementPermutation" = testIsDerangementPermutation

-- To sort all the properties, we simply sort all the defined properties using the propertySort function (after finding the actual definition of the property)
sortAllPropertiesByStrength :: [String] -> [String]
sortAllPropertiesByStrength = sortBy propertySort


exercise5 :: IO ()
exercise5 = do
    putStrLn "\n--- Exercise 5 ---\n"
    putStrLn "Testing the various properties...:\n"
    putStrLn "To make sure we use random values for both the input as the target, we make sure to call randomInput separately for both input and target.\n"
    putStrLn "Note: \"quickCheck $\" can be replaced in the code with \"quickCheck . verbose $\" to verify individual test cases\n"
    
    putStrLn "Testing property 1: Length:"
    quickCheck $ forAll randomInput $ \input -> forAll randomInput $ \target -> prop_derangementLength input target

    putStrLn "\nTesting property 2: no duplicates:"
    quickCheck $ forAll randomInput $ \input -> forAll randomInput $ \target -> prop_derangementNoDuplicates input target
    
    putStrLn "\nTesting property 3: containing same elements:"
    quickCheck $ forAll randomInput $ \input -> forAll randomInput $ \target -> prop_derangementContainsSameElements input target

    putStrLn "\nTesting property 4: is permutation:"
    quickCheck $ forAll randomInput $ \input -> forAll randomInput $ \target -> prop_derangementPermutation input target

    putStrLn "\n\nNone of the tests fail, which gives us some confidence that the properties hold. More test cases and properties may be added to increase confidence, at the cost of more code maintenance.\n\n"

    putStrLn "To test the strength of the properties, we may reuse our thoughts and code from exercise 3 to order the properties for the domain given."
    putStrLn "This did require making some changes, as the code does not freely accept multiple lists as input."
    putStrLn "It is hard to verify quickly that the current implementation for comparing works correctly, and as expected."
    putStrLn "Taking the inputs and targets [[], [1], [1,2], [2,1], [2,3]] leads to the sorting of the properties in the following order:"
    print (sortAllPropertiesByStrength ["testIsDerangementLengthProperty", "testIsDerangementNoDuplicates", "testIsDerangementContainsSameElements", "testIsDerangementPermutation"])
    putStrLn "Based purely on intuition, this does sound correct, as being a permutation is somewhat strict: it entails length and same elements."
    putStrLn ""