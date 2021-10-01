module Exercise9 where
import Data.List

next :: [Bool] -> [Bool]
next [a,b,c,d,e,f] = [b,c,d,e,f,a /= (b && c)]

distance :: [Bool] -> [Bool] -> Int
distance a b
    | next a == b = 1
    | otherwise = 1 + distance (next a) b

inputs :: Int -> [[Bool]]
inputs 0 = [[]]
inputs n = map (True:) (inputs (n-1)) ++ map (False:) (inputs (n-1))

cycleLengths :: [Int]
cycleLengths = map (\i -> distance i i) (inputs 6)

cycles :: [(Int, Int)]
cycles = let l = cycleLengths in
    map (\n -> (n, (length $ filter (==n) l) `div` n)) (nub l)

-- https://wiki.haskell.org/The_Fibonacci_sequence
fib :: [Int]
fib = 2 : 1 : zipWith (+) fib (tail fib)

answer :: Int
answer = product $ map (\(cl, nr) -> (fib!!cl)^nr) cycles

exercise9 :: IO ()
exercise9 = do
    putStrLn "\n--- Bonus problem 209 ---\n"
    print answer
