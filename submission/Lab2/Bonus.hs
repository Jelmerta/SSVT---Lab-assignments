-- Time spent on exercise: 3 hours.

-- This submission requires Data.Scientific to be installed
-- using stack install scientific.

module Bonus where

import Data.Char
import Data.Scientific -- stack install scientific

-- Prime function from lab1.hs.
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

-- Primes function from lab1.hs.
primes :: [Integer]
primes = 2 : filter prime [3..] 

-- Project Euler problem 1: Multiples of 3 or 5
-- Find all the multiples of 3 or 5 below 1000.
multiples :: [Integer]
multiples = [x | x <- [1..1000], x `mod` 3 == 0 || x `mod` 5 == 0]

-- Test idea: for every number in the result of multiples,
-- test if the number divided by three is an integer (and not a float)
-- or the number divided by five is an integer.
testMultiples :: Bool
testMultiples = and [x | y <- multiples, let x = isInteger (fromInteger (y `div` 3)) || isInteger (fromInteger (y `div` 5))]

-- Project Euler problem 2: Even Fibonacci numbers
-- By considering the terms in the Fibonacci sequence whose values do not
-- exceed four million, find the sum of the even-valued terms.
-- This Fibonacci implementation contains ideas from
-- https://wiki.haskell.org/The_Fibonacci_sequence

-- Gives all Fibonacci numbers to 4000000.
fibonaccis :: [Integer]
fibonaccis = takeWhile (<= 4000000) (0 : 1 : zipWith (+) fibonaccis (tail fibonaccis))

-- Gives all even Fibonacci numbers to 4000000.
evenFibonaccis :: [Integer]
evenFibonaccis = filter even fibonaccis

-- Returns true if n is a perfect square.
-- Code from answer to
-- https://stackoverflow.com/questions/16228542/haskell-function-to-test-if-int-is-perfect-square-using-infinite-list/16228642
isSquare :: Integer -> Bool
isSquare n = head (dropWhile (< n) squares) == n
    where squares = [x*x | x <- [0..]]

-- Test if all numbers n given by fibonaccis have the property that
-- 5*n*n+4 or 5*n*n-4 is a perfect square.
testFibonaccis :: Bool
testFibonaccis = and [x | n <- fibonaccis, let x = isSquare (5*n*n+4) || isSquare (5*n*n-4)]

-- Test if all numbers given by the evenFibonaccis are even. We would
-- not consider this as a very good thest because the actual implementation of
-- the evenFibonaccis function already guarantees that the numbers are even
-- using the even function.
testEvenFibonaccis :: Bool
testEvenFibonaccis = all even evenFibonaccis

-- Project Euler problem 357: Prime generating integers
-- Find the sum of all positive integers n not exceeding 100 000
-- such that for every divisor d of n, d+n/d is prime.

-- Gives all divisors of integer n.
divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n], n `mod` x == 0]

-- Returns if d+n/d is prime.
dndIsPrime :: Integer -> Integer -> Bool
dndIsPrime n d = prime $ d + n `div` d

-- Gives all positive integers not exceeding 100 000 such that
-- for every divisor d of n, d+n/d is prime.
pr :: [Integer]
pr = [n | n <- [0..100000], all (dndIsPrime n) (divisors n)]

-- For every number n in the list resulting from pr,
-- check if every divisor d of n, d+n/d is a prime number.
testPr :: Bool
testPr = and [x | n <- pr, let y = divisors n, d <- y, let z = d + n `div` d, let x = prime z]

-- Project Euler problem 377: Let f(n) be the sum of all positive integers
-- that do not have a zero in their digits and have a digital sum equal to n.
-- Find sum_0 ^17 f(13^i).

-- Convert an integer to a list of its digits.
-- https://stackoverflow.com/a/3964069
digits :: Integer -> [Int]
digits = map digitToInt . show

-- True if number has zero in its digits.
zeroInDigits :: Integer -> Bool
zeroInDigits n = 0 `elem` digits n

-- Function f as described in the problem specification.
-- The number 9999 is introduced here to stop generation of
-- an infinite amount of results.
f :: Integer -> Integer
f n = sum [x | x <- [1..9999], not (zeroInDigits x), toInteger (sum (digits x)) == n]

-- Function that finds the sum number for the described problem.
find :: Integer
find = sum $ map (f 13^) [1..17]

-- Main function to start the exercise code.
bonus :: IO ()
bonus = do
    putStrLn "--- Bonus assignments: Project Euler ---"

    putStrLn "\n--- Euler problem 1. ---\n"
    putStrLn "--- Find the sum of all the multiples of 3 or 5 below 1000. ---"
    putStrLn "All the multiples of 3 and 5 below 1000 are:"
    print multiples
    putStrLn "The sum of all the multiples of 3 and 5 below 1000 is:"
    print $ sum multiples
    putStrLn "Test if every result from the multiples function is divisible \
    \by three or five."
    print testMultiples

    putStrLn "\n--- Euler problem 2. ---\n"
    putStrLn "--- By considering the terms in the Fibonacci sequence whose \
    \values do not exceed four million, find the sum of \
    \the even-valued terms. ---"
    putStrLn "The even-valued terms in the Fibonacci sequence whose values \
    \do not exceed four million are:"
    print evenFibonaccis
    putStrLn "The sum of the even-valied terms in the Fibonacci sequence \
    \whose values do not exceed four million are:"
    print $ sum evenFibonaccis
    putStrLn "Test if all numbers given by the Fibonacci function have \
    \the property property of Fibonacci numbers (this takes a while)."
    print testFibonaccis
    putStrLn "Test if all numbers given by evenFibonaccis are even."
    print evenFibonaccis

    putStrLn "\n--- Euler problem 357. ---\n"
    putStrLn "--- Find the sum of all positive integers n not exceeding \
    \100 0000 such that for every divisor d of n, d+n/d is prime. The original \
    \problem asked for numbers up to 100 000 000 but this \
    \took too long to run---"
    putStrLn "The sum is: (this takes a while)"
    print $ sum pr
    putStrLn "For every number n in the list resulting from pr, check if \
    \every divisor d of n, d+n/d is a prime number (this takes a while)."
    print testPr

    putStrLn "\n--- Euler problem 377. ---\n"
    putStrLn "Let f(n) be the sum of all positive integers that do not have a \
    \zero in their digits and have a digital sum equal to n.\n\
    \Find sum_0 ^17 f(13^i)."
    putStrLn "The result of this problem (the sum) is:"
    print find
