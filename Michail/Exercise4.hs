module Exercise4 where
import Lab1
import Distribution.SPDX (LicenseId(XSkat))

-- A function using list comprehension to find all the numbers from 1..10000 that are primes and their reversals are primes aswell
revPrimes :: [Integer]
revPrimes = [x | x <- [1..10000], prime x && prime (reversal x)]

-- Testing the generated list using recursion to determine if the produced numbers meet the criteria
testRevPrimes :: [Integer] -> Bool 
testRevPrimes [] = True 
testRevPrimes (x:xs) | prime x && prime (reversal x) =testRevPrimes xs
                     | otherwise = False 
