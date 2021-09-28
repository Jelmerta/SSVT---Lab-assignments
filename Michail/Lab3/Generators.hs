module Generators where

import Data.List
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

---- Step 3: Distribute Dsj and Cnj to CNF form
------ Apply distributive law for a Disjunction: A V (B Λ C) <--> (A V B) Λ (A V C)

-- NOT MY CODE (from here): 
-- Found this at: https://github.com/cristiantamas/ssvt-labs/blob/master/Lab_3/final_solution/Ex3.hs

isSimpleAtom :: Form -> Bool
isSimpleAtom (Prop x) = True
isSimpleAtom (Neg (Prop x)) = True
isSimpleAtom _ = False

convertToCnf :: Form -> Form
convertToCnf (Prop x) = Prop x
convertToCnf (Neg (Prop x)) = Neg (Prop x)
convertToCnf (Neg (Neg f)) = convertToCnf f
convertToCnf (Dsj[a, b])
    | isSimpleAtom a && isSimpleAtom b = Dsj[a, b]
convertToCnf (Cnj[a, b])
    | isSimpleAtom a && isSimpleAtom b = Cnj[a, b]
convertToCnf (Cnj[f1, f2]) = Cnj[convertToCnf f1, convertToCnf f2]
convertToCnf (Dsj [f1, Dsj fs]) =  convertToCnf (Cnj (map ((\ f -> Dsj [f1, f]) . convertToCnf) fs))
convertToCnf (Dsj [f1, Cnj fs]) =  convertToCnf (Cnj (map ((\ f -> Dsj [f1, f]) . convertToCnf) fs))
convertToCnf (Dsj [Cnj fs, f1]) =  convertToCnf (Cnj (map ((\ f -> Dsj [f, f1]) . convertToCnf) fs))
convertToCnf (Dsj [Dsj fs, f1]) =  convertToCnf (Cnj (map ((\ f -> Dsj [f, f1]) . convertToCnf) fs))
convertToCnf (Dsj [Prop x, f2]) = Dsj [Prop x, f2]

------ Preconditions:
-------- Arrowfree  and nnf input formula
------ Postconditionsc (To test):
-------- only conjuctions for the exterior 
-------- logically equivalent to input formula


-- (Until here) 
-- MY CODE

-- Convert form to CNF form
-- Preconditions 
---- Valid Form : checked by type inference
formToCNF :: Form -> Form 
formToCNF f = convertToCnf $ (nnf . arrowfree) f
