import Lecture3

contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f1 f2 = tautology $ Impl f1 f2

equiv :: Form -> Form -> Bool
equiv f1 f2 = tautology $ Equiv f1 f2
