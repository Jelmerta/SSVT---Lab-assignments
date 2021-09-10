module Exercise3 where
import Test.QuickCheck
import Data.List

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

nrPermutations :: [a] -> Int
nrPermutations a = length $ permutations a

nrPermutations' :: [a] -> Int
nrPermutations' a = factorial $ length a

testNrPermutations :: Integer -> Bool
testNrPermutations n = let a = [1..n`mod`9] in nrPermutations a == nrPermutations' a

exercise3 :: IO ()
exercise3 = do
    putStrLn "--- Exercise 3 ---"
    quickCheckResult testNrPermutations
    putStrLn ""
