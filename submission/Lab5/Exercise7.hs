module Exercise7 where
import Exercise2
import LTS
import Data.List
import Data.Maybe
import Test.QuickCheck

-- Time spent:
-- 4 hours

index :: [State] -> State -> Int
index q s = fromMaybe 0 (elemIndex s q) * 4

character :: Int -> [State] -> LabeledTransition -> String
character n q (s1,l,s2)
    | equal && i1 < i2 = " ▾  "
    | equal && i2 < i1 = " ▴  "
    | between && n == (i1 + i2) `div` 2 = take 3 (l ++ "--") ++ " "
    | between = " │  "
    | otherwise = "    "
    where
        i1 = index q s1
        i2 = index q s2
        equal = n `elem` [i1,i2]
        between = sort [i1,n,i2] !! 1 == n

state :: [State] -> Int -> String
state q n
    | n `mod` 4 == 0 = "  " ++ show (q!!(n `div` 4)) ++ "   "
    | otherwise = "      "

line :: [State] -> [LabeledTransition] -> Int -> String
line q t n = state q n ++ concatMap (character n q) t

visualizeLTS :: IOLTS -> IO ()
visualizeLTS (q,_,_,t,_) = putStr $ unlines $ map (line q t) [0..length q*4-1]

exercise7 :: IO ()
exercise7 = do
    putStrLn "\n--- Exercise 7 ---\n"
    putStrLn "Visualizing the following IOLTS:\n"
    lts <- generate ltsGen
    print lts
    putStrLn "\n"
    putStrLn "STATE\n"
    visualizeLTS lts
