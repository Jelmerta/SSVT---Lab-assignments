module Exercise7 where
import Data.Char
import Data.List

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

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f s (x:xs) = f x : altMap s f xs

luhnDouble :: Int -> Int 
luhnDouble x = if 2*x > 9 then 2*x -9 else 2*x

luhn' :: [Int] -> Bool
luhn' xs = total `mod` 10 == 0
    where total = sum (altMap (+0) (luhnDouble ) (reverse xs))

testLuhns :: Integer -> Bool
testLuhns n = luhn (fromIntegral n) == luhn' (digits (fromIntegral n))

exercise7 :: IO ()
exercise7 = do
    putStrLn "--- Exercise 7 ---\n"
    putStrLn "First luhn solution, results for a valid card number: 79927398713\n"
    print (luhn 79927398713)
    putStrLn "\nSecond luhn' solution, results for a valid card number: 79927398713\n"
    print (luhn' (digits 79927398713))
    putStrLn "\nChecking that both solutions are equal for 5 10 digit numbers that are including valid and invalid card numbers\n"
    print (testLuhns 79927398713) -- Valid card number
    print (testLuhns 79924398713) -- Invalid card number
    print (testLuhns 79927308713) -- Invalid card number
    print (testLuhns 79927398413) -- Invalid card number
    print (testLuhns 79927398710) -- Invalid card number

    putStrLn "\nIdeally, we would have a generator creating random 10 digit numbers\n and then verifying with QuickCheck that testLuhns passes the test for all values."
    
    putStrLn "\n"
