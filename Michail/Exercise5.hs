module Exercise5 where

import Lab1


smallest101Prime :: [Integer]->Integer
smallest101Prime (x:xs) | let a = sum(take 101 (x:xs)) in prime a  = sum(take 101 (x:xs))
                        | otherwise = smallest101Prime xs

what:: Integer
what = sum(take 101 primes)