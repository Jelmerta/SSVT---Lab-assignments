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
-- To find out the derangements of a list, we can calculate all of the permutations of a list and for every index check if the result the element in the same index.

-- To check if an input list is a derangement of a target list we verify that for every index in the list, we find a different element.
-- We stop checking when one of the lists has become empty. If only one of the target lists is empty, this means that there is a mismatch in the amount of elements and therefore this is not a derangement.
-- If both lists and up being empty after being checked for every element being different we know that we're dealing with a derangement
-- TODO WAIT: [1,2,3] and [4,5,6] are not derangements... What do we do then?
-- isDerangement :: (Eq a) => [a] -> [a] -> Bool
-- isDerangement [] [] = True
-- isDerangement list1 [] = False
-- isDerangement [] list2 = False
-- isDerangement (list1Head:list1Tail) list2@(list2Head:list2Tail)
--     | (list1Head == list2Head) || (list1Head `notElem` list2) = False
--     | otherwise = isDerangement list1Tail list2Tail

-- containsSameElements :: (Eq a) => [a] -> [a] -> Bool
-- containsSameElements [] ys = True
-- containsSameElements (x:xs) ys 
--     | x `elem` ys = containsSameElements xs ys
--     | otherwise = False

index :: (Eq a) => a -> [a] -> Integer
index e (x:xs)
    | e == x = 0
    | otherwise = 1 + index e xs


isDerangement :: (Eq a) => [a] -> [a] -> Bool
isDerangement [] [] = True
-- isDerangement (x:xs) ys
--     | x `elem` ys && (index x xs /= index x ys) = isDerangement xs (delete x ys)  -- (list1Head == list2Head) || (list1Head `notElem` list2) = False
--     | otherwise = False
isDerangement xs ys = [x `elem` ys && (index x xs /= index x ys) | x <- xs ]


-- To find out the derangements of a list, we can find out the permutations and simply filter out
-- all permutations that do not match the a derangement according to the function above
-- deran :: Integer -> [[Integer]]
-- deran n = filter (isDerangement [1..n-1]) (permutations [1..n-1])

-- Well-chosen integer lists:
-- []       []      True? If not, it is still useful to have it this way to make calculation easier
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

-- Testing isDerangement:

-- Property 1: If length of list1 does not match length of list 2, it is not a derangement... Exception for empty list?
-- TODO Maybe more mathematical
testIsDerangementLengthProperty :: [Integer] -> [Integer] -> Bool
testIsDerangementLengthProperty list1 list2 = length list1 == length list2

-- Property 2: If element TODO This implies the length of the list as well
testIsDerangementContainsSameElements :: [Integer] -> [Integer] -> Bool
testIsDerangementContainsSameElements [] [] = True
testIsDerangementContainsSameElements [] ys = False
testIsDerangementContainsSameElements xs [] = False
testIsDerangementContainsSameElements (x:xs) ys
    | x `elem` ys = testIsDerangementContainsSameElements xs (delete x ys)
    | otherwise = False

-- Property 3: Positions of elements in list 1 do not match the position of elements in list 2
testIsDerangementElementPosition :: [Integer] -> [Integer] -> Bool
testIsDerangementElementPosition [] [] = True
testIsDerangementElementPosition xs [] = False -- I'm kind of testing more than required: Now we're also making sure the length of the lists match... Probably should not do that, though we need some stopping condition
testIsDerangementElementPosition [] ys = False
testIsDerangementElementPosition (x:xs) (y:ys)
    | x == y = False
    | otherwise = testIsDerangementElementPosition xs ys


-- Things I can think of:
-- Every element in the output list is unique (is this necessarily the case? We don't filter out duplicates). This is probably true, but should also hold for the precondition. As we currently only generate numbers from 1..n-1 this is not a problem.
-- We can recheck using the isDerangement function, though that basically just tests our code. Is a very strong property...


-- Every list in the output will have the same size
-- Checking the output list size:
-- checkDeranPropOutputSize [[2,3,1],[3,1,2]] 3
-- True
-- checkDeranPropOutputSize [[2,3,1],[3,1,2,3]] 3
-- False
-- testDeranPropOutputSize :: Integer -> Bool
-- testDeranPropOutputSize n = checkDeranPropOutputSize (deran n) (n-1)

-- checkDeranPropOutputSize :: [[Integer]] -> Integer -> Bool
-- checkDeranPropOutputSize [] n = True
-- checkDeranPropOutputSize (x:xs) n
--     | n /= fromIntegral(length x) = False
--     | otherwise = checkDeranPropOutputSize xs n


-- Property 2: Every list in the output will contain the same elements as in the input list


exercise5 :: IO ()
exercise5 = do
    putStrLn "\n--- Exercise 5 ---\n\n"