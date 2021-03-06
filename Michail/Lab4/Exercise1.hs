module Exercise1 where

import SetOrd
import System.Random 
import Data.List
import Test.QuickCheck
import Text.XHtml.Transitional (black)

-- Author: Michail Skalistiris Pikis
-- 2 hours


-- Convert list to set
genRandSet :: IO (Set Int)
genRandSet = list2set <$> genRandomList

-- Generate list with of random length and random elements
genRandomList :: IO ([Int])
genRandomList = do
    l <- abs <$> aRandomInt
    aRandomList l


-- Generate list with a number of random elements
aRandomList :: Int -> IO([Int])
aRandomList 0 = return []
aRandomList n = do
    l  <- aRandomInt 
    ls <- aRandomList (n-1)
    return (l:ls)

-- Get a random int x E (-100,100)
aRandomInt :: IO (Int)
aRandomInt = getStdRandom (randomR (-100,100))


quickCheckRandSet :: Gen (Set Int)
quickCheckRandSet = list2set <$> listOf (choose (-100, 100))


quickCheckRandSet2 :: Gen (Set Int, Set Int)
quickCheckRandSet2 = do
    a <- list2set <$> listOf (choose (-100, 100))
    b <- list2set <$> listOf (choose (-100, 100))
    return (a,b)

quickCheckRandSet3 :: Gen (Set Int, Set Int, Set Int)
quickCheckRandSet3 = do
    a <- list2set <$> listOf (choose (-100, 100))
    b <- list2set <$> listOf (choose (-100, 100))
    c <- list2set <$> listOf (choose (-100, 100))
    return (a,b,c)


exercise1 :: IO ()
exercise1 = do
    putStrLn "\n--- Exercise 1 ---\n"
    putStrLn "\nExamples from the custom generator (elements from -100 to 100):\n"
    a <- genRandSet
    b <- genRandSet
    c <- genRandSet
    d <- genRandSet
    e <- genRandSet
    print a
    putStrLn ""
    print b
    putStrLn ""
    print c
    putStrLn ""
    print d
    putStrLn ""
    print e
    putStrLn ""
    putStrLn "\nQuickCheck Set Int generator implementaion:\n\nquickCheckRandSet :: Gen (Set Int)\nquickCheckRandSet = list2set <$> listOf (choose (-100, 100))"
    putStrLn ""

