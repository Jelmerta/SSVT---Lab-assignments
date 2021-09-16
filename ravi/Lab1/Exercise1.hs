module Exercise1 where
import Test.QuickCheck

-- 1^2 + 2^2 + ... + n
squareSum :: Integer -> Integer
squareSum n = sum $ map (^2) [1..n]

-- n(n+1)(2*n+1) / 6
squareSum' :: Integer -> Integer
squareSum' n = div (n*(n+1)*(2*n+1)) 6

-- Checks if 1^2 + 2^2 + ... + n^2 = n(n+1)(2*n+1) / 6 for a certain n
testSquareSum :: Integer -> Bool
testSquareSum n = squareSum (abs n) == squareSum' (abs n)


-- 1^3 + 2^3 + ... + n^3
cubeSum :: Integer -> Integer
cubeSum n = sum $ map (^3) [1..n]

-- (n(n+1)/2)^2
cubeSum' :: Integer -> Integer
cubeSum' n = (div (n*(n+1)) 2)^2

-- Checks if 1^3 + 2^3 + ... + n^3 = (n(n+1)/2)^2 for a certain n
testCubeSum :: Integer -> Bool
testCubeSum n = cubeSum (abs n) == cubeSum' (abs n)


exercise1 :: IO ()
exercise1 = do
    putStrLn "--- Exercise 1 ---"
    putStrLn "Using Quickcheck to test the property 1^2 + 2^2 + ... + n^2\
    \ = n(n+1)(2*n+1)/6 for natural numbers n:"
    quickCheckResult testSquareSum
    putStrLn "\nUsing Quickcheck to test the property 1^3 + 2^3 + ... + n^3\
    \ = (n(n+1)/2)^2 for natural numbers n:"
    quickCheckResult testCubeSum
    putStrLn "\n"
