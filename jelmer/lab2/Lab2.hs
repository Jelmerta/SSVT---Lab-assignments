module Lab2 where
import Lab1
import Data.List
import Test.QuickCheck    

neg :: (a -> Bool) -> a -> Bool
neg p x = not (p x)

infixl 2 .&&.
infixl 2 .||.

(.&&.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .&&. q = \ x -> p x && q x

(.||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
p .||. q = \ x -> p x || q x

-- Provided functions for test properties
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- To compare properties, we can make use of the following code, which will tell us if a property is stronger, weaker or equivalent
compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q
                    qp = stronger xs q p
                in
                    if pq && qp then "equivalent"
                    else if pq  then "stronger"
                    else if qp  then "weaker"
                    else             "incomparable"