module Exercise4 where
import System.Random
import Data.List


-- HELPER FUNCTIONS

-- Get random integer between 0 and n
getRandomInt :: Int -> IO Int
getRandomInt n = getStdRandom (randomR (0,n))

-- Get n random integers in the range 0 - 4
getIntL :: Int -> IO [Int]
getIntL 0 = return []
getIntL n = do
    x <-  getRandomInt 4
    xs <- getIntL (n-1)
    return (x:xs)

-- Generate a random list of max length 4 consisting of numbers in the range 0 - 4
genIntList :: IO [Int]
genIntList = do
    n <- getRandomInt 4
    xs <- getIntL n
    return $ nub xs

-- Precondition/postcondition testing function. Stops after n relevant tests.
test :: Int -> Int -> ([Int] -> [Int] -> Bool) -> ([Int] -> [Int] -> Bool) ->
        (Bool -> Bool) -> IO ()
test k n f pre post = if k == n then print (show n ++ " tests passed")
    else do
        xs <- genIntList
        ys <- genIntList
        if pre xs ys then
            if post (f xs ys) then do
                test (k+1) n f pre post
            else error ("failed test on: " ++ show (xs,ys))
        else
            test k n f pre post


-- IMPLEMENTATION

-- Checks if two lists are a permutation of each other.
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] a = null a
isPermutation (h:t) a = (elem h a) && (isPermutation t (filter (/= h) a))


-- TESTING

-- The fact that we may assume that the input lists don't contain any duplicates,
-- means for our testing procedure that we can use lists that don't contain any
-- duplicates.

-- Some testable properties for this function, sorted from strongest to weakest are:

-- Sould return True when:
-- the two lists are empty
-- the two lists are the same
-- all elements that are in one list are also in the other

-- Should return False when:
-- one list is empty and the other isn't
-- the lists are different lengths
-- an element is in one list but not the other

-- These properties are defined below. We won't test the last mentioned properties
-- of each catagory. because that would entail reimplementing the solution.

bothEmpty :: [a] -> [a] -> Bool
bothEmpty a b = null a && null b

same :: Eq a => [a] -> [a] -> Bool
same a b = a == b

oneEmpty :: [a] -> [a] -> Bool
oneEmpty a b = null a /= null b

differentLength :: [a] -> [a] -> Bool
differentLength a b = length a /= length b

-- Note: For each property the testing stops after 100 relevant and succesful tests
-- have been encountered.
exercise4 :: IO ()
exercise4 = do
    putStrLn "\n--- Exercise 4 ---\n"
    putStrLn "Both lists empty test:"
    test 0 100 isPermutation bothEmpty (==True)
    putStrLn "\nBoth lists same test:"
    test 0 100 isPermutation same (==True)
    putStrLn "\nOne list empty test:"
    test 0 100 isPermutation oneEmpty (==False)
    putStrLn "\nDifferent lengths test:"
    test 0 100 isPermutation differentLength (==False)
