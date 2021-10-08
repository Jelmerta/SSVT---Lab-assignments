module Exercise3 where
import LTS
import Test.QuickCheck

select :: [LabeledTransition] -> State -> [(Label,State)]
select t s = [(l,s2) | (s1,l,s2) <- t, s1 == s]

add :: (Trace,State) -> (Label, State) -> (Trace,State)
add (tr,s1) (l,s2) = (tr ++ [l], s2)

newTraces :: [LabeledTransition] -> (Trace,State) -> [(Trace,State)]
newTraces t (tr,s) = map (add (tr,s)) (select t s)

allNewTraces :: [LabeledTransition] -> [(Trace,State)] -> [(Trace,State)]
allNewTraces t = concatMap (newTraces t)

traces :: [LabeledTransition] -> State -> [(Trace,State)]
traces t s = concat $ takeWhile (/= []) $ iterate (allNewTraces t) [([],s)]

straces :: IOLTS -> [Trace]
straces (_,_,_,t,q0) = [tr | (tr,_) <- traces t q0]

exercise3 :: IO ()
exercise3 = do
    putStrLn "\n--- Exercise 3 ---\n"
