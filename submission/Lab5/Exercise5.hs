module Exercise5 where
import LTS
import Data.List
import Test.QuickCheck
import Exercise4

out :: IOLTS -> State -> [Label]
out (_,s,m,p,_) state = [l | (s0, l, s1) <- p, s0 == state, l `elem` m] ++ [delta | (s0, l, s1) <- p, s0 == state, l `elem` s]
    
outAfter :: IOLTS -> Trace -> [Label]
outAfter iolts trace = nub(concatMap (out iolts) (iolts `after` trace))

-- infix 1 `ioco`
-- ioco :: IOLTS -> IOLTS -> Bool
-- i `ioco` m = 

exercise5 :: IO ()
exercise5 = do
    putStrLn "\n--- Exercise 5 ---\n"
    putStrLn "outAfter tretmanK3 []"
    print(outAfter tretmanK3 [])
    putStrLn "outAfter tretmanK3 [\"but\"]"
    print(outAfter tretmanK3 ["but"])