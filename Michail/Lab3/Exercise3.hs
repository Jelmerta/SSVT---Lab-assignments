module Exercise3 where

import Lecture3
import SetOrd 
import Data.List
import Exercise1

-- 3 hours

-- Write a Haskell program for converting formulas into CNF:

-- CNF Defintitions
-- A literal is an atom p or the negation of an atom ¬p.
-- A clause is a disjunction of literals.
-- A formula C is in conjunctive normal form (or: CNF) if it is a conjunction of clauses.

-- Implementation steps:
---- Step 1: Remove Arrows
------ use the equivalence between p→q and ¬p∨q to get rid of → (Impl) symbols
------ use the equivalence of p↔q and (¬p∨q)∧(p∨¬q) to get rid of ↔ (Equiv) symbols
------ Preconditions:
-------- No
------ Post conditions (To Test):
-------- The result should have no occurrences of Impl and Equiv.
-------- The result should be logically equivalent to the original.

{- 
arrowfree :: Form -> Form 
arrowfree (Prop x) = Prop x 
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) = 
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) = 
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2
-}

-- Test: The result should have no occurrences of Impl and Equiv.

-- Test: The result should be logically equivalent to the original.


---- Step 2: Conversion to negation normal form
------ Use the equivalence between ¬¬ϕ and ϕ,
------ Use the equivalence between ¬(ϕ∧ψ) and ¬ϕ∨¬ψ,
------ Use the equivalence between ¬(ϕ∨ψ)and ¬ϕ∧¬ψ
------ Preconditions:
-------- Arrowfree input formula
------ Postconditionsc (To test):
-------- only atoms are negated in the formula
-------- logically equivalent to input formula

{-
nnf :: Form -> Form 
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)
-}

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

form10 = Impl (Neg(Cnj [Impl p q, Impl q r])) (Impl p r)


exercise3 :: IO ()
exercise3 = do
    putStrLn "\n--- Exercise 3 ---\n"
    putStrLn "Input Form:\n"
    print form10
    putStrLn ""
    putStrLn "Convert to CNF:\n"
    print (formToCNF form10)
    putStrLn ""
    




