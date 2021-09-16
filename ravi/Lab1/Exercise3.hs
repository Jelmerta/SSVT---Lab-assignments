module Exercise3 where
import Test.QuickCheck
import Data.List

-- n!
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- |permutations(A)|
nrPermutations :: [a] -> Int
nrPermutations a = length $ permutations a

-- |A|!
nrPermutations' :: [a] -> Int
nrPermutations' a = factorial $ length a

-- Checks whether |permutations(A)| = |A|! for a certain list A
testNrPermutations :: Integer -> Bool
testNrPermutations n = let a = [1..n`mod`9] in nrPermutations a == nrPermutations' a


exercise3 :: IO ()
exercise3 = do
    putStrLn "--- Exercise 3 ---"
    putStrLn "Using Quickcheck to test the following property of the\
    \ function 'permutations': |permutations(A)| = |A|!"
    quickCheckResult testNrPermutations
    putStrLn "\nThis property was hard to test because the number of permutations\
    \ of a list grows quickly. Therefore we limited the sizes of the input\
    \ lists using modulo."
    putStrLn "\nWhat we're testing here is whether the function 'permutations'\
    \ satisfies part of its specification."
    putStrLn "\n"
