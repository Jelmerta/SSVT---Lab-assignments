module Exercise4 where
import LTS
import Data.List
import Test.QuickCheck
import Exercise3
import Data.Maybe

-- Find list of possible states after performing the current label on the current state
states :: [LabeledTransition] -> Label -> State -> [State]
states transitions label state = catMaybes([states' transition label state | transition <- transitions])

states' :: LabeledTransition -> Label -> State -> Maybe State
states' (s0, l, s1) label state
    | l == label && s0 == state = Just s1
    | otherwise = Nothing

after' :: [LabeledTransition] -> Trace -> State -> [State]
after' _ [] state = [state]
after' transitions (t1:trace) state = concatMap (after' transitions trace) (states transitions t1 state)

infix 1 `after`
after :: IOLTS -> Trace -> [State]
after (_,_,_,t,q0) trace = after' t trace q0


-- Initial implementation... Not correct
-- removeBeforeAll :: [Trace] -> Trace -> [Trace]
-- removeBeforeAll traces trace = map (`removeBefore` trace) traces

-- removeBefore :: Trace -> Trace -> Trace
-- removeBefore [] [] = []
-- removeBefore [] tracePartial = []
-- removeBefore trace [] = trace
-- removeBefore (headFull:traceFull) (headPartial:tracePartial) 
--     | headFull == headPartial = removeBefore traceFull tracePartial
--     | otherwise = headFull:traceFull



exercise4 :: IO ()
exercise4 = do
    putStrLn "\n--- Exercise 4 ---\n"
    -- *Exercise4> after coffeeImpl6 ["coin"]
    -- [[],[],[],["coffee"]]