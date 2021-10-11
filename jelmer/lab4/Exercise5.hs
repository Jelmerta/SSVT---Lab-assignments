-- Time spent: 45minutes

module Exercise5 where

import Data.List
import Exercise4
import Test.QuickCheck
import SetOrd
import Data.Fix

fp :: Eq a => (a -> a) -> a -> a
fp f = until (\ x -> x == f x) f

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- The @@ operator is able to find relations to make two pairs transitive.
-- We want to apply the @@-operator on the union of the original set along with the newly introduced transitive pair(s) until we notice there's no change anymore.
-- The operator for fixed points introduced during the lecture can be used for this.

-- *Exercise5> trClos  [(1,3),(3,4),(3,2)]
-- [(1,3),(3,4),(3,2),(1,4),(1,2)]
trClos :: Ord a => Rel a -> Rel a
trClos = fp (\x -> x `union` (x @@ x))
    
exercise5 :: IO ()
exercise5 = do
    putStrLn "\n--- Exercise 5---\n\n"