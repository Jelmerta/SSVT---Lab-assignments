module Exercise2 where
import Test.QuickCheck
import Data.List

-- |P(A)|
powerSetSize :: [a] -> Int
powerSetSize a = length $ subsequences a

-- 2^|A|
powerSetSize' :: [a] -> Int
powerSetSize' a = 2 ^ length a

-- Tests whether |P(A)| = 2^|A| for a certain list A
testPowerSetSize :: Integer -> Bool
testPowerSetSize n = let a = [1..n`mod`9] in powerSetSize a == powerSetSize' a


exercise2 :: IO ()
exercise2 = do
    putStrLn "--- Exercise 2 ---"
    putStrLn "Using Quickcheck to test the following property of the\
    \ function 'subsequences': |subsequences(A)| = 2^|A|"
    quickCheckResult testPowerSetSize
    putStrLn "\nThis property was hard to test because the number of subsequences\
    \ of a list grows exponentially. Therefore we limited the sizes of the input\
    \ lists using modulo."
    putStrLn "\nWhat we're testing here is whether the function 'subsequences'\
    \ satisfies part of its specification."
    putStrLn "\n"
