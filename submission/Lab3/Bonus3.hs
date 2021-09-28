module Bonus3 where

f :: ([Int],[Int]) -> ([Int],[Int])
f (xs,ys) = ((sum xs):(init xs), (sum $ xs++ys):(init ys))

bonus3 :: IO ()
bonus3 = do
    putStrLn "--- Bonus problem 191 ---\n"
    print $ let (_,(n:_)) = iterate f ([1,1,0],[1,0,0]) !! 30 in n
