module Exercise1 where

import SetOrd
import System.Random
import GHC.Base (Int)

-- Get a random int from -n to n.
randomInt :: Int -> IO Int
randomInt n = getStdRandom (randomR (-n, n))

-- Get a random integer from 0 to n (positive).
randomPosInt :: Int -> IO Int
randomPosInt n = getStdRandom (randomR (0, n))

-- Generate list with n random elements.
randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
    l  <- randomInt 100
    ls <- randomList (n-1)
    return (l:ls)

-- Generate a random sized list (with max size 100).
randomSizeList :: IO [Int]
randomSizeList = do
    l <- randomPosInt 100
    randomList l

-- Generate a random sized set.
randomSet :: IO (Set Int)
randomSet = list2set <$> randomSizeList
