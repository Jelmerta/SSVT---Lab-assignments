module Exercise4 where
import LTS
import Data.List
import Test.QuickCheck
import Exercise3

removeBeforeAll :: [Trace] -> Trace -> [Trace]
removeBeforeAll traces trace = map (`removeBefore` trace) traces

removeBefore :: Trace -> Trace -> Trace
removeBefore [] [] = []
removeBefore [] tracePartial = []
removeBefore trace [] = trace
removeBefore (headFull:traceFull) (headPartial:tracePartial) 
    | headFull == headPartial = removeBefore traceFull tracePartial
    | otherwise = headFull:traceFull

-- TODO Will return endless results in some cases
after :: IOLTS -> Trace -> [Trace]
after iolts = removeBeforeAll (straces iolts)

exercise4 :: IO ()
exercise4 = do
    putStrLn "\n--- Exercise 4 ---\n"
    -- *Exercise4> after coffeeImpl6 ["coin"]
    -- [[],[],[],["coffee"]]