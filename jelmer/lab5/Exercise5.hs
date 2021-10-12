module Exercise5 where
import LTS
import Data.List
import Test.QuickCheck
import Exercise4

-- TODO: No duplicates
out :: IOLTS -> Trace -> [Label]
out iolts trace = findFirstElements (after iolts trace)

findFirstElements :: [Trace] -> [Label]
findFirstElements = map findFirstElement

findFirstElement :: Trace -> Label
findFirstElement [] = delta
findFirstElement (headLabel:trace) = headLabel

exercise5 :: IO ()
exercise5 = do
    putStrLn "\n--- Exercise 5 ---\n"
    