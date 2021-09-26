-- Task: Write a Haskell program for converting formulas into CNF

module Exercise3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3
import Exercise1

-- Definition:
-- A formula is said to be in CNF, or conjunctive normal form, if:
-- 1. It is conjunction of one or more clauses
-- 2. The clauses are disjunctionsof literals
-- Here, a literal is defined to be an atom or the negation of an atom. It is the only place where the not-operator can be used. 

-- Every propositional formula can be converted into CNF. See: https://en.wikipedia.org/wiki/Conjunctive_normal_form
-- To convert a first-order logic formula into CNF, the following steps can be performed:
-- 1. Remove implications and equivalences
--  a. We can remove implications by using equivalences p→q ↔ ¬p∨q, 
--  b. We can remove equivalences by using p↔q ↔ (¬p∨q)∧(p∨¬q)
-- 2. Convert to NNF (negation normal form). A formula is in NNF if the negation operator is only applied to atoms and only contains the conjunction, disjunction and negation operators.
-- Example:  (¬p ∨ q) ∧ p is in NNF.
--  a. We can simplify formulas using the equivalence ¬¬p ↔ p to remove negations.
--  b. We can move negations inward using the equivalence ¬(p∧q) ↔ ¬p∨¬q (De Morgan's Law)
--  c. We can move negations inward using the equivalence ¬(p∨q) ↔ ¬p∧¬q (De Morgan's Law)

-- To verify that the formula is indeed in NNF, we can test several properties of NNF:
-- 1. We can verify that no occurences of Equiv and Impl exist.
-- 2. Logically, both formulas should be equivalent, which could be verified with a truth table.

-- Luckily for us, code is already provided to remove equivalences, implications, and also to make the formula NNF, so we can reuse these.

nnfConvert :: Form -> Form
nnfConvert form = nnf (arrowfree form)


reduceConjunctions :: [Form] -> [Form]
reduceConjunctions [] = []
reduceConjunctions (Cnj c:fs) = reduceConjunctions c ++ reduceConjunctions fs
reduceConjunctions (f:fs) = (f:reduceConjunctions fs)

-- To convert from NNF to CNF, several steps can be taken:
-- 2. Standardize the variables
-- 3. Skolemize the statement
-- 4. Drop all universal quantifiers.
-- 5. Distribute ORs inwards over ANDs: repeatedly replace
-- cnf (Cnj[Dsj[Cnj[p,q],q],q]) Issue with this formula <- good for testing.
cnfConvert :: Form -> Form

-- (Cnj (c1:fs)): cs
-- cnfConvert (Cnj (Cnj c1:cs)) = cnf (Cnj (c1 ++ cs)) IMPORTANT
-- cnfConvert (Cnj cs) = cnf (Cnj [c | c <- cs])
-- cnfConvert (Cnj cs) = Cnj (map cnf cs)
-- TODO We could remove duplicates
-- nnf (Cnj fs) = Cnj (map nnf fs)
-- *(*(+(1 2) +(2 2)) 2)

-- (1 or 2) And (2 or 2) And 2
-- cnfConvert (Cnj fs) = Cnj (map cnfConvert fs)
-- cnfConvert (Cnj [Dsj[f1,f2], f3]) = 

cnfConvert (Dsj [f1, Cnj[f2, f3]]) = Cnj[Dsj[cnf f1, cnf f2], Dsj[cnf f1, cnf f3]]
cnfConvert (Dsj [Cnj[f1,f2], f3]) = Cnj[Dsj[cnf f1, cnf f3], Dsj[cnf f2, cnf f3]]
cnfConvert (Dsj (Dsj d1:ds)) = cnf (Dsj (d1 ++ ds))
cnfConvert (Dsj ds) = Dsj (map cnf ds)
-- cnfConvert (Prop atom) = Prop atom -- An atom stays an atom (literal)
-- cnfConvert (Neg (Prop atom)) = Neg (Prop atom) -- Negative atom stays an atom (also a literal)
-- cnfConvert (Neg (Neg form)) = cnfConvert form -- Two negations cancel each other out
-- cnfConvert (Cnj fs)
    -- | isValidConjunction (Cnj fs) = Cnj fs
-- cnfConvert (Cnj [f1, f2]) = cnfConvert(Cnj[cnfConvert f1, cnfConvert f2])
-- cnfConvert (Dsj fs)
    -- | isValidDisjunction (Dsj fs) = Dsj fs
-- cnfConvert (Dsj [f1, Cnj[f2, f3]]) = cnfConvert (Cnj [Dsj[cnfConvert f1, cnfConvert f2], Dsj[cnfConvert f1, cnfConvert f3]])
-- cnfConvert (Dsj [f1, f2]) = cnfConvert(Dsj[cnfConvert f1, cnfConvert f2])
cnfConvert (Cnj cs) = Cnj (reduceConjunctions (map cnf cs))
cnfConvert f = f


cnf' :: Form -> Form
cnf' form = while (not . isCnf) cnf form -- Use nnf or not?

-- cnfConvert (Cnj [f1, f2]) = Cnj[cnfConvert f1, cnfConvert f2]
-- cnfConvert (Dsj [f1, f2]) = 

-- TODO We learned a method in class with truth tables and negations, what should we use?


-- To make sure that we succeed in finding a CNF, we verify that the found formula matches the following criteria:
-- 1. 
-- And make sure that the input formula and output formula are logically equivalent
cnf :: Form -> Form
cnf f = cnfConvert (nnfConvert (arrowfree f))

testCnf :: Form -> Bool
testCnf f = equiv f (cnf f)


-- Example of just a single clause: isCnf (Dsj[q,Neg p])
-- Example of a formula in CNF: isCnf (Cnj[Dsj[p,Neg q], Dsj[q,Neg p]])
isCnf :: Form -> Bool
isCnf (Prop atom) = True
isCnf (Neg (Prop atom)) = True
isCnf (Dsj form) = isValidDisjunction (Dsj form) -- If there is only one disjunction, we only have to check the disjunction 
isCnf (Cnj form) = isValidConjunction (Cnj form)
isCnf _ = False

-- isCnf (Neg _) = False -- If we find anything negated besides an atom, we know that there is still work to be done
-- isCnf (Cnj fs) = not (any isValidConjunction fs)
-- isCnf (Dsj fs) = not (any isValidConjunction fs)
-- isCnf (Dsj [form1, form2]) = isCnf form2 && isCnf form2  -- For every clause, we make sure that its literals are disjunct, not conjunct
-- isCnf (Dsj (form1:fs)) = isCnf form1 && isCnf (Dsj fs)

-- isCnf (Cnj[form1,form2]) = isCnf form1 && isCnf form2
-- isCnf (Cnj (form1:fs)) = isCnf form1 && isCnf (Cnj fs)

isValidConjunction :: Form -> Bool
isValidConjunction (Cnj fs) = all isValidClause fs
isValidConjunction _ = False

isValidClause :: Form -> Bool
isValidClause f = isValidDisjunction f || isValidLiteral f

isValidDisjunction :: Form -> Bool
isValidDisjunction (Dsj fs) = all isValidLiteral fs
isValidDisjunction _ = False

isValidLiteral :: Form -> Bool
isValidLiteral (Prop atom) = True
isValidLiteral (Neg (Prop atom)) = True
isValidLiteral f = False


-- *(+(*(+(-1 2) +(2 -1)) *(1 -2)) +(*(+(-1 2) +(2 -1)) *(-2 1)))


-- (*(+(-1 2) +(2 -1)) *(1 -2))
-- (*(+(-1 2) +(2 -1)) *(-2 1))


-- *(+(*(1 2) -1) +(*(1 2) -2))

-- Step 1 of iteration:
-- *(+(*(1 2) -1) +(*(1 2) -2))
-- *Exercise3> Cnj[Dsj[Cnj[p,q], Neg p], Dsj[Cnj[p,q], Neg q]]
-- Step 2 of iteration:
-- *(*(+(1 -1) +(2 -1)) *(+(1 -2) +(2 -2)))
-- cnf (Cnj[Dsj[Cnj[p,q], Neg p], Dsj[Cnj[p,q], Neg q]])

-- Step 3:
-- *(+(1 -1) +(2 -1) +(1 -2) +(2 -2))
-- Finally CNF!