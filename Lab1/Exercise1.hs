module Exercise1 where
import Test.QuickCheck

squareSum :: Integer -> Integer
squareSum n = sum $ map (^2) [1..n]

squareSum' :: Integer -> Integer
squareSum' n = div (n*(n+1)*(2*n+1)) 6

testSquareSum :: Integer -> Bool
testSquareSum n = squareSum (abs n) == squareSum' (abs n)


cubeSum :: Integer -> Integer
cubeSum n = sum $ map (^3) [1..n]

cubeSum' :: Integer -> Integer
cubeSum' n = (div (n*(n+1)) 2)^2

testCubeSum :: Integer -> Bool
testCubeSum n = cubeSum (abs n) == cubeSum' (abs n)


exercise1 :: IO ()
exercise1 = do
    putStrLn "--- Exercise 1 ---"
    quickCheckResult testSquareSum
    quickCheckResult testCubeSum
    putStrLn ""
