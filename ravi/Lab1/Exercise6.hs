module Exercise6 where
import Lab1

-- Infinite list of (p_1 * ... * p_n) + 1 for every n.
primeProducts :: [Integer]
primeProducts = map (+1) (scanl1 (*) primes)

-- Finds the smallest non-prime of the form (p_1 * ... * p_n) + 1.
counterExample :: Integer
counterExample = head $ filter (\n -> not (prime n)) primeProducts


exercise6 :: IO ()
exercise6 = do
    putStrLn "--- Exercise 6 ---"
    putStrLn "The smallest non-prime of the form (p_1 * ... * p_n) + 1 is:"
    print counterExample
    putStrLn "\n"
