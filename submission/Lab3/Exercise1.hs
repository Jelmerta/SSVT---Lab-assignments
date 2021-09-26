module Exercise1 where

import Lecture3
import Data.List

-- 3 hours
-- Most of the time was spend in understanding how the functions of Lecture3.hs work and writing down the excersice1 function

-- Definitions from Logic in Action 2016 and Lecture 3 slides

-- Formulas from Workshop 3 exercise 8

---- p V -q:  
form4 = Dsj[p, Neg q]
---- not a tautology, for p = F; q = T falsies it.
---- Satisable, for p = T satises it. 
---- Hence not a contradiction.

---- p Λ -p: .
form5 = Cnj[p, Neg p]
---- a contradiction for neither p = T nor p = F satisfies it
---- Hence not satisfiable 
---- Not a tautology

---- p V -p:
form6 = Dsj[p,Neg p]
---- a tautology, for both p = T and p = F satisfy it
---- hence satisfiable
---- not a contradiction

---- p -> (p V q):
form7 = Impl p (Dsj[p,q])
---- a tautology, for all valuations for p, q satisfy it
---- hence satisfiable 
---- not a contradiction
------ Left side
form7a = p 
------ Right side
form7b = Dsj[p,q]
------ Logical entailmenet applies for form7a -> form7b because of tautology of form7
------ Logical entailment does not apply for form7b -> form7a for values q = T, p = F

---- (p V q) -> p:
form8 = Impl(Dsj[p, q]) p
---- not a tautology for p = F, q = T falsifies it
---- not a contradiction for p = T, q = T satisfies it
---- hence satisfiable

---- (p -> q) <-> ((-q) -> (-p))
form9 = Equiv (Impl p q)(Impl(Neg q)(Neg p))
---- a tautology for all valuation of p,q
---- satisfiable
---- hence not a contradiction
------ Left side
form9a = Impl p q
------ Right side
form9b = Impl(Neg q)(Neg p)



-- SATISFIABLE
-- Formula f is satisfiable if some valuation makes it true:
satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)

-- CONTRADICTION
-- A form that get the value False for every valuation
contradiction :: Form -> Bool
contradiction f = not (any (\ v -> evl v f) (allVals f))

-- TAUTOLOGY
-- A form that gets the value True for every valuation
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- LOGICAL ENTAILMENT 
-- B logically entails A is true if and only if all the valuations that satisfy B also satisfy A.
entails :: Form -> Form -> Bool
-- p→q and ¬p∨q
-- DNF Form: entails f g = all (\v -> evl v (Dsj[Neg f,g]))(allVals (Dsj[Neg f,g]))
entails f g = all (\v -> evl v (Impl f g))(allVals (Impl f g))

-- LOGICAL EQUIVALENCE
-- A and B are equivalent
equiv :: Form -> Form -> Bool
-- p↔q and (¬p∨q)∧(p∨¬q)
-- CNF Form: equiv f g = all (\ v -> evl v (Cnj[Dsj[Neg f,g],Dsj[f,Neg g]]))(allVals (Cnj[Dsj[Neg f,g],Dsj[f,Neg g]]))
equiv f g = all (\v -> evl v (Equiv f g))(allVals (Equiv f g))

exercise1 :: IO ()
exercise1 = do
    putStrLn "\n--- Exercise 1 ---\n"
    putStrLn "A analysis of the valuations of the forms 4-9 is included in the comments section line 13-57\n"
    putStrLn "- Check 1: Contradiction\n"
    putStrLn "form4 (p V -q): form4 = Dsj[p, Neg q]"
    putStrLn ""
    print (contradiction form4)
    putStrLn ""
    putStrLn "form5 (p Λ -p): form5 = Cnj[p, Neg p]"
    putStrLn ""
    print (contradiction form5)
    putStrLn ""
    putStrLn "form6 (p V -p): form6 = Dsj[p,Neg p]"
    putStrLn ""
    print (contradiction form6)
    putStrLn ""
    putStrLn "form7 (p -> (p V q)):form7 = Impl p (Dsj[p,q])"
    putStrLn ""
    print (contradiction form6)
    putStrLn ""
    putStrLn "- Check 2: Tautology\n"
    putStrLn "form4 (p V -q): form4 = Dsj[p, Neg q]"
    putStrLn ""
    print (tautology form4)
    putStrLn ""
    putStrLn "form5 (p Λ -p): form5 = Cnj[p, Neg p]"
    putStrLn ""
    print (tautology form5)
    putStrLn ""
    putStrLn "form6 (p V -p): form6 = Dsj[p,Neg p]"
    putStrLn ""
    print (tautology form6)
    putStrLn ""
    putStrLn "form7 (p -> (p V q)): form7 = Impl p (Dsj[p,q])"
    putStrLn ""
    print (tautology form7)
    putStrLn ""
    putStrLn "form8 ((p V q) -> p): form8 = Impl(Dsj[p, q]) p"
    putStrLn ""
    print (tautology form8)
    putStrLn ""
    putStrLn "form9 ((p -> q) <-> ((-q) -> (-p))): form9 = Equiv (Impl p q)(Impl(Neg q)(Neg p))"
    putStrLn ""
    print (tautology form9)
    putStrLn ""
    putStrLn "- Check 3: Logical Entailment\n"
    putStrLn "form7a (p): form7a = p"
    putStrLn ""
    putStrLn "form7b (p V q): form7b = Dsj[p,q]"
    putStrLn ""
    putStrLn "Logical entailment: form7a -> form7b"
    putStrLn ""
    print (entails form7a form7b)
    putStrLn ""
    putStrLn "Logical entailment: form7b -> form7a"
    putStrLn ""
    print (entails form7b form7a)
    putStrLn ""
    putStrLn "- Check 4: Logical Equivalence\n"
    putStrLn "form9a (p -> q): form9a = Impl p q"
    putStrLn ""
    putStrLn "form9b (p V q): form9b = Dsj[p,q]"
    putStrLn ""
    putStrLn "Logical equivalence: form9a <-> form9b"
    putStrLn ""
    print (equiv form9a form9b)
    putStrLn ""
    putStrLn "Logical equivalence: form5 (contradiction) <-> form6 (tautology)"
    putStrLn ""
    print (equiv form5 form6)
    putStrLn ""

    