-- Task: Write a Haskell program for converting formulas into CNF
-- Approx time spent: 7h

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

-- Next, to convert from NNF to CNF we have to distribute the ORs inwards over AND.
-- This means we have to replace formulas (p∨q)∧r with (p∨q)∧(p∨r). These are logically equivalent.
-- We want to make sure we don't have multiple disjunctions or conjunctions in a row, such as (p∧q)∧r, as the functions used for disjunctions and conjunctions support lists of forms instead of just two forms.
-- We didn't do this, but an improvement could also be to make sure there are no duplicates.
cnfConvert :: Form -> Form
cnfConvert (Cnj cs) = Cnj (reduceConjunctions (map cnf cs)) -- Conjunctions are made smaller as explained above.
cnfConvert (Dsj [d]) = d -- Base case for disjunction if it only contains one element due to reduction
-- cnfConvert (Dsj ((Cnj c):ds)) = Cnj [(Dsj [c, d]) | d <- ds] -- Might have to play around with arbitrary position for Cnj more...
-- cnfConvert (Dsj (Cnj (c:cs):ds)) = Cnj [Dsj [c,d] | d <- ds]
cnfConvert (Dsj (Dsj d1:ds)) = cnf (Dsj (d1 ++ ds))
cnfConvert (Dsj (f1: Cnj[f2, f3]: f4)) = Dsj (Cnj[Dsj[cnf f1, cnf f2], Dsj[cnf f1, cnf f3]]: f4) -- Making sure that lists longer than 2 elements also work by checking first 2 elements and keeping the rest of the list in place. If a conjunction exists later in the list, this might not be found...
cnfConvert (Dsj ((Cnj[f1,f2]: f3 : f4))) = Dsj (Cnj[Dsj[cnf f1, cnf f3], Dsj[cnf f2, cnf f3]] : f4)
cnfConvert (Dsj ds) = Dsj (reduceDisjunctions (map cnf ds))
cnfConvert f = f -- Anything else, such as a literal, stays the same after conversion

reduceConjunctions :: [Form] -> [Form]
reduceConjunctions [] = []
reduceConjunctions (Cnj c:fs) = reduceConjunctions c ++ reduceConjunctions fs
reduceConjunctions (f:fs) = (f:reduceConjunctions fs)

-- Should only be used if there are no more conjunctions in the disjunction...
reduceDisjunctions :: [Form] -> [Form]
reduceDisjunctions [] = []
reduceDisjunctions (Dsj d:fs) = reduceDisjunctions d ++ reduceDisjunctions fs
reduceDisjunctions (f:fs) = (f:reduceDisjunctions fs)

-- Performs one iteration to find a CNF. Often, we do not find the CNF in one iteration.
cnf :: Form -> Form
cnf f = cnfConvert (nnfConvert (arrowfree f))

-- To make sure we do find a CNF, we loop until we make sure the resulting formula is in CNF.
cnf' :: Form -> Form
cnf' form = while (not . isCnf) cnf form

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


-- Around here is where Ex 4 starts.
-- Generating a random formula can be done by generating a tree structure of forms.
-- We will recursively create forms until every node is an atom. It is important that we create enough chance for it to reach the stopping condition, otherwise we will create an endless form.
-- At every depth (and therefore every subtree) we will choose an operator or atom to stop iteration.

-- Dsj and Cnj require a list of forms. For this, a separate generator is introduced to make sure the lists of forms are relevant
-- This does introduce a circular dependency between formListGen and the generator for Forms, which is probably not all too great.
formListGen :: Gen [Form]
formListGen = frequency [(1, return []), (1, liftM2 (:) arbitrary formListGen)] 

-- Example output of sample formGen:
-- (0==>(0<=>+(0 0 0)))
-- 2
-- *(0 *(4 (*()==>2) +(4 -3 -3)))
-- *()
-- -5
-- *(7)
-- (-5==>(-8<=>-8))
-- -7
-- ((*()==>(-1==>(*(*() +(3) (2==>15) *(+() *(12)) 1)<=>+(-7))))==>+())
-- (3==>(15<=>6))
-- -14
-- As you may be able to tell: 
-- formGen produces empty lists, and lists of 1. Cnj and Dsj of 0/1 are not interesting (or even incorrect).
-- This can be improved by always returning at least 2 forms, with a low chance to add more.
formGen :: Gen Form
formGen = frequency [(6, liftM Prop arbitrary),
    (1, liftM Neg arbitrary),
    (1, liftM Cnj formListGen),
    (1, liftM Dsj formListGen),
    (1, liftM2 Equiv arbitrary arbitrary),
    (1, liftM2 Impl arbitrary arbitrary)]

instance Arbitrary Form where
    arbitrary = formGen

-- formListGen' :: Gen [Form]
-- formListGen' = frequency [(1, return []), (6, liftM2 (:) arbitrary formListGen)]

-- instance Arbitrary [Form] where
--     arbitrary = formListGen

-- formList :: Gen [Form]
-- formList = sized $
--     \n -> do
--         k <- choose (0,n)
--         sequence [arbitrary | _ [1..k]]

-- instance Arbitrary a => Arbitrary [a] where
--     arbitrary = formListGen 0

-- Override the function for list generation in QuickCheck so that we do not create massive lists
-- instance Arbitrary [Form] where
    -- arbitrary = oneof [return [], liftM2 (:) arbitrary arbitrary]
-- arbFormList 0 = liftM  []
-- arbFormList n = liftM2 (:) (arbForm n-1) (arbFormList n-1) 

-- instance Arbitrary a => Arbitrary (Form a) 


-- instance Arbitrary a => Arbitrary Form a where
-- arbitrary (Cnj [p,q])
--  	 liftM Leaf arbitrary
--   liftM Br
-- generate
-- arbitrary elements in it
-- class A

-- TODO Generate a tree and check using QuickCheck

-- newRand = randomIO :: IO Int

-- print(newRand)
-- formGenerator :: Int -> Int -> Form
-- formGenerator depth operation = formGenerator' 0 depth operation



-- formGenerator :: Int -> Form
-- formGenerator depth = do
--     -- randomNumber <- randomIO :: Integer
--     randomNumber <- randomRIO (1,6)
--     return formGenerator' 0 depth randomNumber
--     -- return form

formGenerator' :: Int -> Int -> Int -> Form
formGenerator' currentDepth maxDepth operation
    | (currentDepth == maxDepth) || (operation == 1 && currentDepth <= maxDepth) = Prop currentDepth -- Single atom, if we reach the last depth, we have to put an atom.
    -- | randomNumber == 2 && currentDepth <= maxDepth = do -- Negation of atom
    --     form <- formGenerator (currentDepth - 1)
    --     return (Neg form)
    -- | randomNumber == 3 && currentDepth <= maxDepth = do
    --         -- NOTE We could do this for longer with a chance to add an additional conjunction
    --         formFirst <- formGenerator (currentDepth - 1)
    --         formSecond <- formGenerator (currentDepth - 2)
    --         return (Cnj [formFirst, formSecond])
    -- | randomNumber == 4 && currentDepth <= maxDepth = do 
    --         formFirst <- formGenerator (currentDepth - 1)
    --         formSecond <- formGenerator (currentDepth - 2)
    --         return (Dsj [formFirst, formSecond])
    -- | randomNumber == 5 && currentDepth <= maxDepth = do
    --         formFirst <- formGenerator (currentDepth - 1)
    --         formSecond <- formGenerator (currentDepth - 2)
    --         return (Impl [formFirst, formSecond]) 
    -- | randomNumber == 6 && currentDepth <= maxDepth = do
    --             formFirst <- formGenerator (currentDepth - 1)
    --             formSecond <- formGenerator (currentDepth - 2)
    --             return (Equiv [formFirst, formSecond])


-- To make sure that we succeed in finding a CNF, we verify that the found formula matches the following criteria:
-- 1. 
-- And make sure that the input formula and output formula are logically equivalent
testCnf :: Form -> Bool
testCnf f = equiv f (cnf' f)

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
    print (cnf' (Equiv p q))
    putStrLn "Verifying if this is CNF:"
    print (isCnf (cnf' (Equiv p q)))
    putStrLn "Verifying these are equivalent:"
    print (testCnf (Equiv p q))
    putStrLn "\n\nWe can also find the CNFs of the given formules in Lecture3.hs:"
    putStrLn "Form1:"
    print (form1)
    print (cnf' form1)
    print (isCnf (cnf' form1))
    print (testCnf (form1))
    putStrLn "Form2:"
    print (form2)
    print (cnf' form2)
    print (isCnf (cnf' form2))
    print (testCnf (form2))
    putStrLn "Form3:"
    print (form3)
    print (cnf' form3)
    print (isCnf (cnf' form3))
    print (testCnf (form3))
    putStrLn "We believe there are still some small errors and optimizations possible."
    putStrLn "Not all formulas will work, especially with longer disjunction of conjunctions."
    putStrLn "Duplicates may remain in the final formula and is not the minimal form"

    -- quickCheck $ forAll randomInput $ \input -> forAll randomInput $ \target -> prop_derangementPermutation input target

    -- TODO: Quickcheck our random generator along with some properties (could just be the isCnf function, along with the equivalence check, other properties can also be checked.)