module Exercise5 where

import Lab1

-- Used the tail of the 
smallest101List :: [Integer]->Integer
smallest101List (x:xs) | let a = sum(take 101 (x:xs)) in prime a  = sum(take 101 (x:xs))
                        | otherwise = smallest101List xs
smallest101Primes :: Integer
smallest101Primes = smallest101List primes



-- Going backwards
-- There should be at least one prime, produced by the sum of 101 primes, that is smaller than the one the software produces
-- Perfom test to check all the sums of 101 primes, where every prime is smaller than the one produced by the previous algorithm

-- getting all the relevant primes
relPrimes = [x | x <- [2..smallest101Primes], prime x]


