-- stack --install-ghc runghc

import Lab1
import Test.QuickCheck

cutCheckDigit :: Integer -> Integer
cutCheckDigit n = n `div` 10

getCheckDigit :: Integer -> Integer
getCheckDigit n = n `mod` 10

-- Turn number into a list of every digit
-- Code from answer of https://stackoverflow.com/questions/3963269/split-a-number-into-its-digits-with-haskell
numToDigitsList :: Integral x => x -> [x]
numToDigitsList 0 = []
numToDigitsList x = numToDigitsList (x `div` 10) ++ [x `mod` 10]

-- Turn digit list of numbers into 2d list of every digit, for digits of len 2 (after doubling).
numToDigitsList2 :: Integral x => [x] -> [[x]]
numToDigitsList2 x = map numToDigitsList x

-- Apply function to every other list element.
-- From (with a little change) answer to https://codereview.stackexchange.com/questions/110830/double-every-2nd-item-in-a-list
mapSecond :: (a -> a) -> [a] -> [a]
mapSecond f = zipWith ($) (cycle [f, id])

doubleSecondFromRight :: [Integer] -> [Integer]
doubleSecondFromRight = reverse . mapSecond (*2) . reverse

sumDigits :: [[Integer]] -> [Integer]
sumDigits list = map sum list

sumToCheckDigit :: Integer -> Integer
sumToCheckDigit sum = (10 - (sum `mod` 10))

luhn :: Integer -> Bool
luhn input = sumToCheckDigit ( sum (sumDigits (numToDigitsList2 (doubleSecondFromRight (numToDigitsList (cutCheckDigit input)))))) == getCheckDigit input