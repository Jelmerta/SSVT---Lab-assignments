module Exercise4 where
import LTS

-- Time spent:
-- 1 hour

afterTrace :: [LabeledTransition] -> [State] -> Trace -> [State]
afterTrace t s (l:ls) = afterTrace t ([s2 | (s1,l1,s2) <- t, s1 `elem` s && l1 == l]) ls
afterTrace t s [] = s

infix 1 `after`
after :: IOLTS -> Trace -> [State]
after (_,_,_,t,q0) tr = afterTrace t [q0] tr

exercise4 :: IO ()
exercise4 = do
    putStrLn "\n--- Exercise 4 ---\n"
    putStrLn "*Exercise4> after coffeeImpl6 []"
    print (after coffeeImpl6 [])
    putStrLn "*Exercise4> after coffeeImpl6 [\"coin\"]"
    print (after coffeeImpl6 ["coin"])
    putStrLn "*Exercise4> after coffeeImpl6 [\"coin\", \"coffee\"]"
    print (after coffeeImpl6 ["coin", "coffee"])
