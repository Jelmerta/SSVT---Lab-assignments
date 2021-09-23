module Exercise4 where

import Lecture3
import Data.List
import Exercise1
import Exercise3

-- Create a function to generate all forms with a given depth and number of variables
forms :: Integer -> Integer -> String -> [String]
forms depth vars op
    | op == "-" = [op ++ f | f <- fs]
    | elem op ["*","+"] = [op ++ "(" ++ f1 ++ " " ++ f2 ++ ")" | f1 <- fs, f2 <- fs]
    | elem op ["==>","<=>"] = ["(" ++ f1 ++ op ++ f2 ++ ")" | f1 <- fs, f2 <- fs]
    where fs = allForms (depth - 1) vars

allForms :: Integer -> Integer -> [String]
allForms 1 vars = map show [1..vars]
allForms depth vars = ["*", "+", "-", "==>", "<=>"] >>= forms depth vars

generateForms :: [Form]
generateForms = concat [parse form | form <- (allForms 3 2)]

-- Post conditions (To Test)
---- The result should have no occurrences of Impl and Equiv.
------ We can test this by filtering the form for Impl and Equiv

{-
testNotImplEquiv :: [Forms] -> Bool
testNotImpEquiv | filter()
-}

---- Only atoms are negated in the formula
------ Test this by checking if there is a Neg just before a () or other symbol
---- Logically equivalent to input formula
------ We can test this by using the function equiv from exercise1


testEquivForms :: [Form] -> Bool
testEquivForms [] = True
testEquivForms (f:fs) | equiv f (formToCNF f) = testEquivForms fs
                  | otherwise = False



exercise4 :: IO ()
exercise4 = do
    putStrLn "\n--- Exercise 4 ---\n"
    putStrLn "Test equivalence of 1314 original forms and their corresponding CNF forms:\n"
    print (testEquivForms generateForms)
    putStrLn ""