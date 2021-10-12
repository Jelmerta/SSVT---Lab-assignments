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
(_,_,_,t,q0) `after` trace = after' t trace q0

exercise4 :: IO ()
exercise4 = do
    putStrLn "\n--- Exercise 4 ---\n"
    putStrLn "*Exercise4> after coffeeImpl6 []"
    print (after coffeeImpl6 [])
    putStrLn "*Exercise4> after coffeeImpl6 [\"coin\"]"
    print (after coffeeImpl6 ["coin"])
    putStrLn "*Exercise4> after coffeeImpl6 [\"coin\", \"coffee\"]"
    print (after coffeeImpl6 ["coin", "coffee"])