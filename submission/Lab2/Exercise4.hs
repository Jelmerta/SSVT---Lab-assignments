module Exercise4 where

-- Checks if two lists are a permutation of each other.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] a = null a
isPermutation (h:t) a = isPermutation t (filter (/= h) a)

-- Properties:
-- Should return True on two empty lists
-- Should return False on an empty and nonempty list
-- Should return False if the lists are different lengths
-- Should return False if an element is in one list but not the other
-- Should return True if all elements that are in one list are also in the other.

exercise4 :: IO ()
exercise4 = do
    putStrLn "--- Exercise 4 ---\n"
    print $ isPermutation [3,5,8,2] [2,8,5,3]
