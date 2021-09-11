module Exercise7 where
import Data.Char

-- Double every second number in the list.
double :: [Int] -> [Int]
double [] = []
double [a] = [a]
double (h1:h2:t) = h1 : 2*h2 : (double t)

-- Convert an integer to a list of its digits.
-- https://stackoverflow.com/a/3964069
digits :: Int -> [Int]
digits = map digitToInt . show

-- Calculates the checksum.
checkSum :: [Int] -> Int
checkSum a = sum $ map (sum . digits) (double a)

-- Calculates the checkdigit.
checkDigit :: [Int] -> Int
checkDigit a = 10 - (checkSum a) `mod` 10

-- Checks whether an input satisfies the Luhn formula.
luhn :: Int -> Bool
luhn n = let d = digits n in last d == (checkDigit $ init $ d)


-- To test the luhn-function we are going to generate 10 digit numbers and use a different implementation for the luhn-function'.
-- We are going to run QuickCheck and check the equality of the results of the two luhn-fuctions for any given 10 digit number.

exercise7 :: IO ()
exercise7 = do
    putStrLn "--- Exercise 7 ---"
    print (luhn 79927398713)
    putStrLn "\n"
