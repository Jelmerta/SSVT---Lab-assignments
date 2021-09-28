module Exercise6 where

import Data.List
import Lecture3
import Generators
import Test.QuickCheck

-- 3 hours


-- Cnf forms

form11 = Cnj [(Dsj [p, q]), (Dsj [Neg p, q]), (Dsj [q, Neg q])]

-- Generates CNF forms  (using the generateForms from Exercise4 and translating each form to teh corresponding CNF form)
generateCnf :: [Form]
generateCnf = [formToCNF form | form <- generateForms]

-- Declare types
type Clause  = [Int]
type Clauses = [Clause]

-- CNF to CLS parser 
-- Preconditions: 
---- Form in CNF

cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg (Prop x)) = [[-x]]
cnf2cls (Dsj fs) = [concat (foldMap cnf2cls fs)]
cnf2cls (Cnj fs) = foldMap cnf2cls fs


-- Postconditions:
---- Property 1: The number of conjucted clauses of the CNF form should be the same as the number of elements in the clauses

------ Counts the number of clauses in CNF
-------- Precondition: Input should be a valid CNF Form
conjLength:: Form -> Int
conjLength (Cnj con) = sum[conjLength cl | cl <- con]
conjLength fCnf = 1

------ Counts the number of elements in clauses
-------- Precondition: Input should be a valid CNF Form
clausesLength :: Form -> Int
clausesLength fCnf = length (cnf2cls fCnf)

------ Test the equality of conjuctions for multiple forms
-------- Precondition: Input should be a valid CNF Form
testConj :: [Form] -> Bool
testConj [] = True
testConj (f:fs) | conjLength f == clausesLength f = testConj fs
                  | otherwise = False 

---- Property 2: The number of literals in the CNF form should be the same as the literals in clauses

------ Counts the atoms in the CNF form
-------- Precondition: Input should be a valid CNF Form
literalsCount :: Form -> Int
literalsCount (Dsj dsj) = length dsj
literalsCount (Cnj con) = sum[literalsCount clause | clause <- con]
literalsCount fCnf = 1

------ Counts the sum of the number of elements in every clause of a clauses
-------- Precondition: Input should be a valid CNF Form
everyClauseLength :: Form -> Int
everyClauseLength fCnf = sum[length clause | clause <- cnf2cls fCnf]

------ Test the equality of conjuctions for multiple forms
testDisj :: [Form] -> Bool
testDisj [] = True
testDisj (f:fs) | literalsCount f == everyClauseLength f = testDisj fs
                  | otherwise = False 

---- Property 3: The valuation of the Form and the clause should be the same
------ Create a function that translates a clause to a cnf or that is able to evaluate a Clauses
------ check the equivalence (with equiv from Exercise1) of the cls and the cnf




exercise6 :: IO ()
exercise6 = do
    putStrLn "\n--- Exercise 6 ---\n"
    putStrLn "- CNF formula to Clauses\n"
    putStrLn "CNF Form:\n"
    print form11
    putStrLn ""
    putStrLn "Clauses:\n"
    print (cnf2cls form11)
    putStrLn ""
    putStrLn "- Generator: CNF Formula generator (generateCnf) creating 1314 forms (building upon the generator in Generators.hs):\n"
    putStrLn "- Test 1: The number of conjucted clauses of the CNF form should be the same as the number of elements in Clauses\n"
    print (testConj generateCnf)
    putStrLn ""
    putStrLn "- Test 2: The number of literals in a CNF form should be the same as the literals in Clauses\n"
    print (testDisj generateCnf)
    putStrLn ""




