module Exercise4 where


import Data.List
import Exercise1
import Exercise3
import Lecture3

-- Create a function to generate all forms with a given depth and number of variables
allForms :: Integer -> Integer -> [String]
allForms 1 vars = map show [1..vars]
allForms depth vars = ["*", "+", "-", "==>", "<=>"] >>= forms depth vars

forms :: Integer -> Integer -> String -> [String]
forms depth vars op
    | op == "-" = [op ++ f | f <- fs]
    | elem op ["*","+"] = [op ++ "(" ++ f1 ++ " " ++ f2 ++ ")" | f1 <- fs, f2 <- fs]
    | elem op ["==>","<=>"] = ["(" ++ f1 ++ op ++ f2 ++ ")" | f1 <- fs, f2 <- fs]
    where fs = allForms (depth - 1) vars

generateForms :: [Form]
generateForms = concat [parse form | form <- (allForms 3 2)]

-- Post conditions (To Test)
---- The result should have no occurrences of Impl and Equiv.

checkNotImplEquiv :: Form -> Bool
checkNotImplEquiv (Prop x) = True
checkNotImplEquiv (Neg x) = checkNotImplEquiv x
checkNotImplEquiv (Cnj fs) = all checkNotImplEquiv fs
checkNotImplEquiv (Dsj xs) = all checkNotImplEquiv xs
checkNotImplEquiv _ = False

testNotImplEquiv :: [Form] -> Bool
testNotImplEquiv [] = True
testNotImplEquiv (x:xs) | checkNotImplEquiv (formToCNF x)  = testNotImplEquiv xs
                        | otherwise = False


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
    putStrLn "Formula generator (generateForms) creating all 1314 of depth 3 with 2 variables:\n"
    putStrLn "Test 1: if the output is free of Impl or Equiv:\n"
    print (testNotImplEquiv generateForms)
    putStrLn ""
    putStrLn "Test 3: equivalence of the original and the corresponding CNF form:\n"
    print (testEquivForms generateForms)
    putStrLn ""