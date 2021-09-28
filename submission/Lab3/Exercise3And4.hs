-- Task: Write a Haskell program for converting formulas into CNF
-- Approx time spent: 10h

module Exercise3And4 where

import Data.List
import System.Random
import System.IO.Unsafe
import Test.QuickCheck
import Lecture3
import Exercise1
import System.Random
import Control.Monad


-- Definition:
-- A formula is said to be in CNF, or conjunctive normal form, if:
-- 1. It is a conjunction of one or more clauses
-- 2. The clauses are disjunctionsof literals
-- Here, a literal is defined to be an atom or the negation of an atom. It is the only place where the not-operator can be used. 

-- Every propositional formula can be converted into CNF. See: https://en.wikipedia.org/wiki/Conjunctive_normal_form
-- To convert a propositional logic formula into CNF, we have to convert the formula to NNF, this can be done as follows:
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
-- 2. Logically, both formulas (input and output of nnf) should be equivalent, which could be verified with a truth table. We can use the function we defined in exercise for equivalence for this.

-- Luckily for us, code is already provided to remove equivalences, implications, and also to make the formula NNF, so we can reuse these.
nnfConvert :: Form -> Form
nnfConvert form = nnf (arrowfree form)

-- To convert to CNF, two methods are implemented. The first method is incomplete and resulted in more complex formulas to not ever result in anything. It works fine for smaller formulas.

-- Start of method 1: Direct conversion from from the formula to CNF
-- To convert directly, we have to convert the formula to NNF using the provided methods, and then to CNF by distributing the ORs inwards over AND, as explained on the aforementioned wikipedia page.
-- This means we have to replace formulas (p∨q)∧r with (p∨q)∧(p∨r). These are logically equivalent.
-- We want to make sure we don't have multiple disjunctions or conjunctions in a row, such as (p∧q)∧r, as the functions used for disjunctions and conjunctions support lists of forms instead of just two forms.
-- We didn't do this, but an improvement could also be to make sure there are no duplicates.
cnfConvert :: Form -> Form
cnfConvert (Cnj cs) = Cnj (reduceConjunctions (map cnf1 cs)) -- Conjunctions are made smaller as explained above.
cnfConvert (Dsj [d]) = d -- Base case for disjunction if it only contains one element due to reduction
-- cnfConvert (Dsj ((Cnj c):ds)) = Cnj [(Dsj [c, d]) | d <- ds] -- Might have to play around with arbitrary position for Cnj more... Right now we only reduce the conjunctions if they are at the start of a list
-- cnfConvert (Dsj (Cnj (c:cs):ds)) = Cnj [Dsj [c,d] | d <- ds]
cnfConvert (Dsj (Dsj d1:ds)) = cnf1 (Dsj (d1 ++ ds))
cnfConvert (Dsj (f1: Cnj[f2, f3]: f4)) = Dsj (Cnj[Dsj[cnf1 f1, cnf1 f2], Dsj[cnf1 f1, cnf1 f3]]: f4) -- Making sure that lists longer than 2 elements also work by checking first 2 elements and keeping the rest of the list in place. If a conjunction exists later in the list, this might not be found... Not sure how to resolve this.
cnfConvert (Dsj ((Cnj[f1,f2]: f3 : f4))) = Dsj (Cnj[Dsj[cnf1 f1, cnf1 f3], Dsj[cnf1 f2, cnf1 f3]] : f4)
cnfConvert (Dsj ds) = Dsj (reduceDisjunctions (map cnf1 ds))
cnfConvert f = f -- Anything else, such as a literal, stays the same after conversion

reduceConjunctions :: [Form] -> [Form]
reduceConjunctions [] = []
reduceConjunctions (Cnj c:fs) = reduceConjunctions c ++ reduceConjunctions fs
reduceConjunctions (f:fs) = f:reduceConjunctions fs

-- Should only be used if there are no more conjunctions in the disjunction...
reduceDisjunctions :: [Form] -> [Form]
reduceDisjunctions [] = []
reduceDisjunctions (Dsj d:fs) = reduceDisjunctions d ++ reduceDisjunctions fs
reduceDisjunctions (f:fs) = f:reduceDisjunctions fs

-- Performs one iteration to find a CNF. Often, we do not find the CNF in one iteration.
cnf1 :: Form -> Form
cnf1 f = cnfConvert (nnfConvert (arrowfree f))

-- To make sure we do find a CNF, we loop until we make sure the resulting formula is in CNF.
-- To test individual formulas from text-input we can use something like this: map cnf' (parse "*(3 4)")
cnf1' :: Form -> Form
cnf1' = while (not . isCnf) cnf1

-- End of method 1

-- Start of method 2
-- Method 2 was explained in class: We can convert a truth table to CNF.
-- We can fairly easily construct a truth table from the original function.
-- The following steps have to be taken to convert a truth table to CNF:
-- Step 1: Take all rows in the truth table that lead to a false result.
-- Step 2: Reverse all the input literals: p becomes Neg p, Neg p becomes p.
-- Step 3: Take the conjunction of the disjuncted reversed literals.

-- Takes one entry in the valuation and inverse the negation (positive becomes negative, negative becomes positive)
negateAtomsInClause :: (Name, Bool) -> Form
negateAtomsInClause (name, val)
    | val = Neg (Prop name) -- atom is true, so negate the atom
    | otherwise = Prop name -- atom is false, so just return the atom

convertRowToClause :: Valuation -> Form
convertRowToClause row
    | length row == 1 = negateAtomsInClause (head row)
    | otherwise = Dsj $ negateAtomsInClause <$> row

-- Evaluate the possible rows (valuations) for the formula and only retrieve the ones that are false
falseRows :: Form -> [Valuation]
falseRows form = filter (\val -> not (evl val form)) (allVals form)

cnf2 :: Form -> Form
cnf2 f 
    | ((length (falseRows f)) == 1) = convertRowToClause (head (falseRows f))
    | otherwise = Cnj (convertRowToClause <$> falseRows f)

-- End of method 2

-- To find out if the formula is CNF, every part of it is checked. All conjunctions should only contain disjunctions or literals. All conjunctions should only contain literals. Anything else is not CNF.
-- Example of just a single clause: isCnf (Dsj[q,Neg p])
-- Example of a formula in CNF: isCnf (Cnj[Dsj[p,Neg q], Dsj[q,Neg p]])
isCnf :: Form -> Bool
isCnf (Prop atom) = True
isCnf (Neg (Prop atom)) = True
isCnf (Dsj form) = isValidDisjunction (Dsj form) -- If there is only one disjunction, we only have to check the disjunction 
isCnf (Cnj form) = isValidConjunction (Cnj form)
isCnf _ = False

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
isValidLiteral _ = False


-- Exercise 4
-- Generating a random formula can be done by generating a tree structure of forms.
-- We will recursively create forms until every node is an atom. It is important that we create enough chance for it to reach the stopping condition, otherwise we will create an endless form.
-- At every depth (and therefore every subtree) we will choose an operator or atom to stop iteration.

-- Dsj and Cnj require a list of forms. For this, a separate generator is introduced to make sure the lists of forms are relevant
-- This does introduce a circular dependency between formListGen and the generator for Forms, which is probably not all too great.
-- To make sure all lists have at least two arguments, we make use of the liftM2 operator to find two arbitrary forms. This could probably be simplified somehow...
-- Then, there is a chance another value will be added to the list to make sure we do not receive only 2 arguments.
formListGen :: Gen [Form]
formListGen = frequency [(4, liftM2 (:) arbitrary (liftM2 (:) arbitrary (return []))), (1, liftM2 (:) arbitrary formListGen)] 

-- Example output of sample formGen (one of the better ones, often it will have a single Prop, or very long formulas)
-- 0
-- (((-1<=>-2)==>+(2 --2))<=>0)
-- (-3<=>-2)
-- *(-3 (0<=>-4) 1)
-- ((+((-3==>(1==>(4==>+((-8<=>+(-1 3)) 5 -3 5)))) 0)<=>4)<=>2)
-- (6==>*(6 1))
-- (-7==>-11)
-- +(-10 14 4)
-- +(+(-5 16) *(-7 *(2 8)) 3)
-- 4
-- --15
formGen :: Gen Form
formGen = frequency [(16, liftM Prop arbitrary), -- HLinter mentions we can use fmap instead of liftM. liftM works fine for us. 
    (1, liftM Neg arbitrary),
    (1, liftM Cnj formListGen),
    (1, liftM Dsj formListGen),
    (1, liftM2 Equiv arbitrary arbitrary),
    (1, liftM2 Impl arbitrary arbitrary)]

instance Arbitrary Form where
    arbitrary = formGen

-- To make sure that we succeed in finding a CNF, we should verify that the found formula matches the following criteria:
-- 1. The resulting formula is equivalent with the input formula
-- 2. The resulting formula is in CNF
testCnf1 :: Form -> Bool
testCnf1 f = equiv f (cnf1' f) -- Currently only checking equivalence, cnf1' implicitly checks until the formula is in CNF.

testCnf2 :: Form -> Bool
testCnf2 f = equiv f (cnf2 f) && (isCnf (cnf2 f))

exercise3And4 :: IO ()
exercise3And4 = do
    putStrLn "\n--- Exercise 3 and 4---\n\n"
    putStrLn "Testing CNF for p ↔ q"
    putStrLn "Converting to NNF gives:"
    print (nnfConvert (arrowfree (Equiv p q)))
-- Step 1 of CNF iteration:
-- *(+(*(1 2) -1) +(*(1 2) -2)), which is the same as Cnj[Dsj[Cnj[p,q], Neg p], Dsj[Cnj[p,q], Neg q]]
-- Step 2 of iteration:
-- *(*(+(1 -1) +(2 -1)) *(+(1 -2) +(2 -2))), which is the same as (Cnj[Dsj[Cnj[p,q], Neg p], Dsj[Cnj[p,q], Neg q]])
-- Step 3:
-- *(+(1 -1) +(2 -1) +(1 -2) +(2 -2))
-- Finally CNF!
    putStrLn "Converting to CNF gives:"
    print (cnf1' (Equiv p q))
    putStrLn "Verifying if this is CNF:"
    print (isCnf (cnf1' (Equiv p q)))
    putStrLn "Verifying these are equivalent:"
    print (testCnf1 (Equiv p q))
    putStrLn "\n\nWe can also find the CNFs of the given formules in Lecture3.hs:"
    putStrLn "Form1:"
    print (form1)
    print (cnf1' form1)
    print (isCnf (cnf1' form1))
    print (testCnf1 (form1))
    putStrLn "Form2:"
    print (form2)
    print (cnf1' form2)
    print (isCnf (cnf1' form2))
    print (testCnf1 (form2))
    putStrLn "Form3:"
    print (form3)
    print (cnf1' form3)
    print (isCnf (cnf1' form3))
    print (testCnf1 (form3))
    putStrLn "We believe there are still some small errors and optimizations possible."
    putStrLn "Not all formulas will work, especially with longer disjunction of conjunctions."
    putStrLn "Duplicates may remain in the final formula and is not the minimal form"

    putStrLn "\n\nExample output of the form generator:"
    sample formGen
    -- Right now some functions cannot be turned into CNF. We have to figure out when this happens. It works fine for smaller functions.
    -- May run several cases successfully, but won't resolve larger, complexer formulas.
    -- verboseCheck $ forAll formGen $ \input -> isCnf (cnf1' input)

    -- The new method may also result in quickCheck running forever, which is likely caused by an explosion in input arguments options from allVals.
    -- To make sure this doesn't happen, we toned down the formula creation quite a bit to create smaller formulas on average. Though this often results in very simple dull formulas.
    -- With this generator it is very easy to get either very small or very large formulas: it is difficult to find a middle ground. Improvements could be made in this area.
    -- We believe this will likely work on larger formulas, but simply take too long too compute.
    putStrLn "\n\nChecking cnf2 implementation using quickcheck"
    quickCheck testCnf2