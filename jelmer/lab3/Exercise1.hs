module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

-- The definitions for the logical notions can be defined as follows using the forms defined in Lecture3.hs:
contradiction :: Form -> Bool
contradiction form = not (any (\ v -> evl v form) (allVals form)) -- TODO not any

-- Testing manually:
-- p = Prop 1
-- q = Prop 2
-- tautology (Dsj [q, Neg q])


-- satisfiable f = any (\ v -> evl v f) (allVals f)

printInputs :: Form -> [Valuation] -> [String]
printInputs form evaluations = [(show(eval)) | eval <- evaluations]

printEvals :: Form -> [Valuation] -> [String]
printEvals form evaluations = [(show(evl eval form)) | eval <- evaluations]

printTruthTable :: Form -> [(String, String)]
printTruthTable form1 = zip (printInputs form1 (allVals form1)) (printEvals form1 (allVals form1))

tautology :: Form -> Bool
tautology form = all (\ v -> evl v form) (allVals form)

-- logicalEntailment -  B logically entails A is true if and only if all the valuations thatsatisfy B also satisfy A. (As found in the lecture notes)
entails :: Form -> Form -> Bool
entails form1 form2 = all (\ v -> evl v (Impl form1 form2)) (allVals (Impl form1 form2)) -- TODO Use Impl?

-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv form1 form2 = all (\ v -> evl v (Equiv form1 form2)) (allVals (Equiv form1 form2))

-- To make sure these functions work correctly, we can make a truth table with an expected value and verify that we get the right outputs.

exercise1 :: IO ()
exercise1 = do
    putStrLn "\n--- Exercise 1 ---\n\n"