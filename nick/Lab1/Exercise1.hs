-- stack --install-ghc runghc

import Lab1
import Test.QuickCheck

func2  :: Integer -> Integer
func2 n = sum (map (^2) [1..n])

func2' :: Integer -> Integer
func2' n = (n*(n+1)*(2*n+1)) `div` 6

func3 :: Integer -> Integer
func3 n = sum (map (^3) [1..n])

func3' :: Integer -> Integer
func3' n = (n*(n+1) `div` 2)^2

prop_func2Equals :: Integer -> Bool
prop_func2Equals n = let a = abs n in func2 a == func2' a

prop_func3Equals :: Integer -> Bool
prop_func3Equals n = let a = abs n in func3 a == func3' a

main :: IO ()
main = do
    quickCheck prop_func2Equals
    quickCheck prop_func3Equals
