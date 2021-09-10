module Exercise2 where
import Test.QuickCheck
import Data.List

powerSetSize :: [a] -> Int
powerSetSize a = length $ subsequences a

powerSetSize' :: [a] -> Int
powerSetSize' a = 2 ^ length a

testPowerSetSize :: Integer -> Bool
testPowerSetSize n = let a = [1..n`mod`9] in powerSetSize a == powerSetSize' a


exercise2 :: IO ()
exercise2 = do
    putStrLn "--- Exercise 2 ---"
    quickCheckResult testPowerSetSize
    putStrLn ""
