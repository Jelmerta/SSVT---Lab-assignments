module Bonus3 where

f :: ([Int],[Int]) -> ([Int],[Int])
f (xs,ys) = ((sum xs):(init xs), (sum $ xs++ys):(init ys))

bonus3 :: IO ()
bonus3 = do
    putStrLn "\n--- Euler problem 191 ---\n"
    putStrLn "How many prize strings exist over a 30-day period?\n"

    print $ let (_,(n:_)) = iterate f ([1,1,0],[1,0,0]) !! 30 in n

    putStrLn "The function could be tested by creating a bruteforce solution to the problem and then comparing the two functions for low input values."
