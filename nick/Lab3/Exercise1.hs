module Exercise1 where

import Lecture3

-- Satisfiable code from lecture nodes.
satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = any (\ v -> (evl v f && not evl v f)) (allVals f)

-- A formula f is a tautology if it is satisfied by all valuations.
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- logical entailment
-- B logically entails A is true if and only if all the valuations thatsatisfy B also satisfy A.
entails :: Form -> Form -> Bool
entails f1 f2 = all (\ v -> evl v f1) (allVals f1) && all (\ v -> evl v f2) (allVals f1)

-- logical equivalence
-- A and B are logically equivalent if A entails B and B entails A.
equiv :: Form -> Form -> Bool
equiv f1 f2 = entails f1 f2 && entails f2 f1