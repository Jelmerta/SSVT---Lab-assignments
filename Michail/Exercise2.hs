import Test.QuickCheck
import Debug.Trace   
import System.Directory.Internal.Prelude (Int)
import GHC.Integer (Integer)
import Data.ByteString.Lazy (putStrLn)
import Data.List
import Data.List (subsequences)
import GHC.Base (Bool)

---- Excercise 2 - Wk1Ex4

-- Defined the set and a powerset to check the equality of |P(A)| = n^2 and generated multiple positive integers including 0. 
-- Checked the base case. The quickcheck never reached 100 because the sets elements grow exponentially which makes takes a lot of processing power.
-- The algorithm is checking if the the given relation is true for the cardinalities of the two sets but not veryfing the mathematical equality.

--Creating a type Set
type Set a = [a]

-- The power
set :: Integer -> Set Integer
set n = [x | x <- [0..n-1]]

powerset :: Integer -> Set (Set Integer)
powerset n = subsequences (set n)

testSets :: Integer -> Bool
testSets n = length (powerset n) == 2 ^ length (set n)

-- Generate natural numbers
genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)