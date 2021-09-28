module Bonus2 where

indices :: [(Int,Int)]
indices = [(i,j) | i <- [0..9], j <- [0..9]]

initial :: [Int]
initial = map (\(i,j) -> fromEnum (i > 0 && i+j <= 9)) indices

targets :: (Int,Int) -> [(Int,Int)]
targets (i,j) = [(k,i) | k <- [0..(9-i-j)]]

calcSum :: [Int] -> [(Int,Int)] -> Int
calcSum xs targets = sum $ map (\(i,j) -> xs !! (10*i+j)) targets

f :: [Int] -> [Int]
f xs = map ((calcSum xs) . targets) indices


bonus2 :: IO ()
bonus2 = do
    putStrLn "--- Bonus problem 164 ---"
    print $ sum $ iterate f initial !! 18
