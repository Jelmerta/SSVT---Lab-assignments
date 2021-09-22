import Lecture3

contradiction :: Form -> Bool
contradiction f = not $ satisfiable f

tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

entails :: Form -> Form -> Bool
entails f1 f2 = all (\v -> (evl v f1) --> (evl v f2)) (allVals $ Cnj [f1,f2])

equiv :: Form -> Form -> Bool
equiv f1 f2 = all (\v -> (evl v f1) == (evl v f2)) (allVals $ Cnj [f1,f2])
