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
testIsDerangementLengthProperty :: [Integer] -> [Integer] -> Property
testIsDerangementLengthProperty xs ys = isDerangement xs ys ==> length xs == length ys

-- Property 2: If list a is a derangement of list b, neither will have duplicate elements
testIsDerangementNoDuplicates :: [Integer] -> [Integer] -> Property
testIsDerangementNoDuplicates xs ys = isDerangement xs ys ==> (length (nub xs) == length xs)
                                                            && (length (nub ys) == length ys)
-- Property 3: Elements in the derangements are also in the original
testIsDerangementContainsSameElements :: [Integer] -> [Integer] -> Property
testIsDerangementContainsSameElements xs ys = isDerangement xs ys ==> (sum [fromEnum(x `elem` ys) | x <- xs]) == length xs

-- Property 4: Derangements are permutations of each other
testIsDerangementPermutation :: [Integer] -> [Integer] -> Property
testIsDerangementPermutation xs ys = isDerangement xs ys ==> isPermutation xs ys

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

exercise5 :: IO ()
exercise5 = do
    putStrLn "\n--- Exercise 5 ---\n\n"
    putStrLn "Testing the various properties...:\n"
    putStrLn "To make sure we use random values for both the input as the target, we make sure to call randomInput separately for both input and target.\n"
    putStrLn "Note: \"quickCheck $\" can be replaced in the code with \"quickCheck . verbose $\" to verify individual test cases\n"
    
    putStrLn "Testing property 1: Length:"
    quickCheck $ forAll randomInput $ \input -> forAll randomInput $ \target -> testIsDerangementLengthProperty input target

    putStrLn "\nTesting property 2: no duplicates:"
    quickCheck $ forAll randomInput $ \input -> forAll randomInput $ \target -> testIsDerangementNoDuplicates input target
    
    putStrLn "\nTesting property 3: containing same elements:"
    quickCheck $ forAll randomInput $ \input -> forAll randomInput $ \target -> testIsDerangementContainsSameElements input target

    putStrLn "\nTesting property 4: is permutation:"
    quickCheck $ forAll randomInput $ \input -> forAll randomInput $ \target -> testIsDerangementPermutation input target

    putStrLn "\n\nNone of the tests fail, which gives us some confidence that the properties hold. More test cases and properties may be added to increase confidence, at the cost of more code maintenance.\n\n"

    putStrLn "To test the strength of the properties, we may reuse our thoughts and code from exercise 3 to order the properties for the domain given."
    putStrLn "Sadly, this is not easily usable with the way the properties are defined right now using the quickCheck properties and requires some changes."