module Exercise4 where


import Data.List
import Exercise1
import Exercise3
import Lecture3
import Data.Bool (Bool(False))


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

-- Inspired by: 

-- Postconditions (To Test)
---- Property 1: The result should have no occurrences of Impl or Equiv.

------ Returns false if there is an occurance of Equiv or Impl
checkNotImplEquiv :: Form -> Bool
checkNotImplEquiv (Prop x) = True
checkNotImplEquiv (Neg x) = checkNotImplEquiv x
checkNotImplEquiv (Cnj fs) = all checkNotImplEquiv fs
checkNotImplEquiv (Dsj xs) = all checkNotImplEquiv xs
checkNotImplEquiv _ = False

------ Test for multiple forms
testNotImplEquiv :: [Form] -> Bool
testNotImplEquiv [] = True
testNotImplEquiv (x:xs) | checkNotImplEquiv (formToCNF x)  = testNotImplEquiv xs
                        | otherwise = False


---- Property 2: Only atoms are negated in the formula
------ Test this by checking if there is a Neg just before Cnj or Dsj

-------- Returns false if there is an occurance of Dsj or Cnj in a Neg
checkNoNeg :: Form -> Bool
checkNoNeg (Prop x) = True
checkNoNeg (Neg (Dsj xs)) = False
checkNoNeg (Neg (Cnj xs)) = False
checkNoNeg (Neg x) = True
checkNoNeg (Cnj fs) = all checkNoNeg fs
checkNoNeg (Dsj xs) = all checkNoNeg xs
checkNoNeg _ = False

------ Test for multiple forms
testNoNeg :: [Form] -> Bool
testNoNeg [] = True
testNoNeg (x:xs) | checkNoNeg (formToCNF x)  = testNoNeg xs
                        | otherwise = False


---- Property 3: Disjunctions are only between literals

---- Property 4: Logically equivalent to input formula
------ We can test this by using the function equiv from exercise1

------ Test for multiple forms
testEquivForms :: [Form] -> Bool
testEquivForms [] = True
testEquivForms (f:fs) | equiv f (formToCNF f) = testEquivForms fs
                  | otherwise = False



exercise4 :: IO ()
exercise4 = do
    putStrLn "\n--- Exercise 4 ---\n"
    putStrLn "Test 1: the output formulas are free of Impl or Equiv:\n"
    print (testNotImplEquiv generateForms)
    putStrLn ""
    putStrLn "Test 2: Neg(s) in the output formulas contain only an atom:\n"
    print (testNoNeg generateForms)
    putStrLn ""
    putStrLn "Test 4: equivalence of each input formula with the output formula:\n"
    print (testEquivForms generateForms)
    putStrLn ""