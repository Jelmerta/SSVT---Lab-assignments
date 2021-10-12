module Exercise3 where
import LTS
import Test.QuickCheck
import Exercise2

-- Time spent:
-- 3 hours

-- Gives the states a state can go to and the labels that take it there
select :: [LabeledTransition] -> State -> [(Label,State)]
select t s = [(l,s2) | (s1,l,s2) <- t, s1 == s]

-- Add a label to the trace and update the new end state
add :: (Trace,State) -> (Label, State) -> (Trace,State)
add (tr,s1) (l,s2) = (tr ++ [l], s2)

-- Determines the new traces created from the given trace
newTraces :: [LabeledTransition] -> (Trace,State) -> [(Trace,State)]
newTraces t (tr,s) = map (add (tr,s)) (select t s)

-- Determines the new traces created from a list of traces
allNewTraces :: [LabeledTransition] -> [(Trace,State)] -> [(Trace,State)]
allNewTraces t = concatMap (newTraces t)

-- Generate all traces by repearedly created new ones from the existing ones
traces :: [LabeledTransition] -> State -> [(Trace,State)]
traces t s = concat $ takeWhile (/= []) $ iterate (allNewTraces t) [([],s)]

-- Returns all traces
straces :: IOLTS -> [Trace]
straces (_,_,_,t,q0) = [tr | (tr,_) <- traces t q0]

-- Generate a random trace
traceGen :: Gen Trace
traceGen = do lts <- ltsGen
              tr <- elements $ take 100 $ straces lts
              return tr

exercise3 :: IO ()
exercise3 = do
    putStrLn "\n--- Exercise 3 ---\n"
    putStrLn "As an example, here are the traces of the following LTS:\n"
    print coffeeImpl3
    putStrLn "\nTraces:"
    print $ straces coffeeImpl3
    putStrLn "\nAnd here is a random trace generated from a random LTS:"
    trace <- generate traceGen
    print trace
