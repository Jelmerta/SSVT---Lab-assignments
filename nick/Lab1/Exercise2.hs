-- stack --install-ghc runghc

import Lab1
import Data.List
import Test.QuickCheck

lengthAllSubsets :: Integer -> Integer
lengthAllSubsets n = toInteger (length (subsequences [1..n]))

func :: Integer -> Integer
func n = 2^n

propLength :: Integer -> Bool
propLength n = let a = abs n in lengthAllSubsets a == func a

-- Property is hard to test because the type of length is [a] -> Int, this cant be directly compared to Integer.
-- Also because the generated lists become very long, so it takes a lot of time, therefore quickcheck is only performed 33 times.

main :: IO ()
main = do
    quickCheck (withMaxSuccess 33 propLength)