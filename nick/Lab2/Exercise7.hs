import Data.Char (digitToInt, isDigit, ord)
import Data.Digits (unDigits)
import Data.String

-- TODO: check for correct IBAN length per country
-- (https://nl.iban.com/structuur).

iban :: String -> Bool
iban code = 1 == ibanComputeRemainder (integerListToString (ibanConvertToInteger (ibanRearrange code)))

ibanCountry :: String -> String
ibanCountry = take 2

-- Rotate a list n steps.
-- https://stackoverflow.com/questions/16378773/rotate-a-list-in-haskell
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

-- Move the four initial characters to the end of the string.
ibanRearrange :: String -> String
ibanRearrange = rotate 4

-- Convert a character to the number specified by the IBAN specifications.
charToInteger :: Char -> Integer
charToInteger letter = if isDigit letter then toInteger (digitToInt letter) else toInteger (ord letter - 55)

-- Replace each letter in the string with two digits, thereby expanding the string, where A = 10, B = 11, ..., Z = 35.
ibanConvertToInteger :: [Char] -> [Integer]
ibanConvertToInteger = map charToInteger 

-- Convert list of integers to a string.
integerListToString :: [Integer] -> String
integerListToString = concatMap show

-- Interpret the string as a decimal integer and compute the remainder of that number on division by 97.
ibanComputeRemainder :: String -> Integer
ibanComputeRemainder code = read code `mod` 97

exercise7 :: IO ()
exercise7 = do
    putStrLn "--- Exercise 7: Implementing and testing IBAN validation ---"

    putStrLn "Validate IBAN from example of\nhttps://en.wikipedia.org\
    \/wiki/International_Bank_Account_Number#Validating_the_IBAN"
    print $ iban "GB82WEST12345698765432"
    