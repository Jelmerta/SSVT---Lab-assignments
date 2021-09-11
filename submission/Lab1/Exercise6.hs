module Exercise6 where
import Lab1

-- Infinite list of (p_1 * ... * p_n) + 1 for every n.
primeProducts :: [Integer]
primeProducts = map (+1) (scanl1 (*) primes)

-- To refute the conjecture, we need to find a list of counterexamples.
-- To do this, we produce the list of non-prime number results of the formula (p_1 * ... * p_n) + 1.
counterExamples :: [Integer]
counterExamples = filter (not . prime) primeProducts


exercise6 :: IO ()
exercise6 = do
    putStrLn "--- Exercise 6 ---\n"
    putStrLn "The smallest non-prime of the form (p_1 * ... * p_n) + 1 is:"
    print (head counterExamples)
    putStrLn "\n"
