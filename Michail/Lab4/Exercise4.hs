module Exercise4 where

import SetOrd
import Exercise3
import Data.List

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial a r = all (\x -> any (\y -> (x,y) `elem` r)a) a



