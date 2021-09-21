module Exercise6 where

-- Time spent: 1 hour 

-- The ROT13 is a simpler version of the Ceasars Cipher
-- It moves every letter character of a string by 13 places
-- It is also its own inverse (encoder & decoder) as ROT13(ROT13 "id") = "id"
-- It should take as an input a string of any characters and of any length
-- It should output the same string with every letter (Upper && lower) characters moved by 13 places

-- ROT13 algorithm
-- Part of the solution is based on Chapter 5 of the Programming with Haskell book
import Data.Char
import Data.List
import Test.QuickCheck
import GHC.Base (String)


-- transforming letter to integer
letInt :: Char -> Int
letInt c = ord c - ord 'a'

-- transforming integer to letter
intLet :: Int -> Char
intLet n = chr (ord 'a' + n)

-- shift by 13 places 
shift13 :: Char -> Char
shift13  c | isLower c = intLet ((letInt c + 13) `mod` 26)
          | isUpper c = toUpper (intLet ((letInt (toLower c) + 13) `mod` 26))
                    | otherwise = c

-- count how occurances of a char in string
count :: Char -> String -> Int
count x xs = length [x | x' <- xs , x' == x]

-- the function
rot13 :: String -> String
rot13 xs = [shift13 x | x <- xs]

-- The preconditions for the rot13 function: 
---- the input is a string which is secured by type inference
---- The string should not contain symbols because they might be interpeted as functions by Haskell

-- The postconditions for the function:
---- 1. The input string and the output string should have the same length
---- 2. The encoder rot13 also decodes (inverse function) the string

-- Random string generator

lowers ::[Char]
lowers = ['a'..'z']

uppers :: [Char]
uppers = ['A'..'Z']

randomString :: Gen String
randomString = listOf $ elements (lowers ++ uppers)

-- Test 1
checkLength :: String -> Bool 
checkLength xs = (length xs) == (length (rot13 xs))


-- Test 2:
-- We can prove this if the application of ROT13 to ROT13 "string" will return "string"
checkInverse :: String -> Bool
checkInverse xs = xs == (rot13(rot13 xs))

exercise6 :: IO ()
exercise6 = do
    putStrLn "\n--- Exercise 6 ---"
    putStrLn "\nTest 1: if the input string and the output string have the same length\n"
    quickCheckResult $ forAll randomString checkLength
    putStrLn "\nTest 2: if the function is also able to decode the encoded message \n"
    quickCheckResult $ forAll randomString checkInverse
    putStrLn ""
