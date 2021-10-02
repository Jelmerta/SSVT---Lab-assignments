module Exercise9 where
import Data.List

-- Bonus problem 209: Circular Logic

-- A k-input binary truth table is a map from k input bits (binary digits,
-- 0 [false] or 1 [true]) to 1 output bit.

-- How many 6-input binary truth tables, τ, satisfy the formula
-- τ(a, b, c, d, e, f) AND τ(b, c, d, e, f, a XOR (b AND c)) = 0
-- for all 6-bit inputs (a, b, c, d, e, f)?


-- Solution:

-- The input space consists of 64 binary tuples of length 6.
-- Let's first create a function to generate these tuples:

inputs :: Int -> [[Bool]]
inputs 0 = [[]]
inputs n = map (True:) (inputs (n-1)) ++ map (False:) (inputs (n-1))

-- Next we define a 'next' function between these tuples:
-- next((a,b,c,d,e,f)) = (b, c, d, e, f, a XOR (b AND c))

next :: [Bool] -> [Bool]
next [a,b,c,d,e,f] = [b,c,d,e,f,a /= (b && c)]

-- Something to notice about this function is that when applied repeatedly to
-- some initial tuple, it will eventually reach this same tuple again.

-- This is because the function has exactly one inverse, namely:
-- prev((m,n,o,p,q,r)) = (r XOR (m AND n),m,n,o,p,q)

-- This implies that the 'next' function divides the input space into a number
-- of distinct cycles. To find the lengths of these cycles, we first define a
-- distance function.

distance :: [Bool] -> [Bool] -> Int
distance a b
    | next a == b = 1
    | otherwise = 1 + distance (next a) b

-- Now the following function determines, for each of the 64 input tuples, the
-- length of the cycle it belongs to.

cycleLengths :: [Int]
cycleLengths = map (\i -> distance i i) (inputs 6)

-- Obviously different tuples can belong to the same cycle. The output of
-- cycleLengths contains 12 occurrences of the number 6 for example. This means
-- that there are 2 cycles of length 6. The following function is therefore
-- used to give the number of cycles there are of each cycle length. So it will
-- contain (6, 2) for example.

cycles :: [(Int, Int)]
cycles = let l = cycleLengths in
    map (\n -> (n, (length $ filter (==n) l) `div` n)) (nub l)

-- Now that we have the cycle lengths, we can get back to the original problem.
-- It states that for an input tuple I:
-- τ(I) AND τ(next(I)) = 0
-- This means that τ(I) and τ(next(I)) can't both be 1.

-- If we think about the cycles in terms of outputs instead of inputs, it means
-- that these output cycles can't have any consecutive 1's in them. We're
-- interested in the number of such configurations for each cycle.

-- So how many binary strings of size n are there without any consecutive 1's
-- (including the first and last element)? It turns out to be the Lucas numbers,
-- a variant of the Fibonacci numbers. We'll just use the one-liner from the
-- Haskell Wiki for this https://wiki.haskell.org/The_Fibonacci_sequence.

luc :: [Int]
luc = 2 : 1 : zipWith (+) luc (tail luc)

-- Now that we can calculate the number of valid output configurations for
-- each cycle, we just have to multiply these numbers together to get the
-- number of valid output configurations for the entire truth table.

answer :: Int
answer = product $ map (\(cl, nr) -> (luc!!cl)^nr) cycles

exercise9 :: IO ()
exercise9 = do
    putStrLn "\n--- Bonus problem 209 ---\n"
    putStrLn "How many 6-input binary truth tables, τ, satisfy the formula\n\
    \τ(a, b, c, d, e, f) AND τ(b, c, d, e, f, a XOR (b AND c)) = 0\n\
    \for all 6-bit inputs (a, b, c, d, e, f)?\n"
    print answer
