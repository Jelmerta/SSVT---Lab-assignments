module Exercise7 where

import Data.List

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f s (x:xs) = f x : altMap s f xs

-- Ex 10
double :: Int -> Int 
double x = x+x

luhnDouble :: Int -> Int 
luhnDouble x = if double x > 9 then double x -9 else double x

luhn :: [Int] -> Bool
luhn xs = if total `mod` 10 == 0 then True else False
    where total = sum (altMap (+0) (luhnDouble ) (reverse xs))