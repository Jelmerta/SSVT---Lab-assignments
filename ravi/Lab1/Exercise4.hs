module Exercise4 where
import Lab1

-- Finds all primes < 10000 that are also primes when reversed.
mirrorPrimes :: [Integer]
mirrorPrimes = [n | n <- filter prime [1..10000], prime $ Lab1.reversal n]

-- Tests whether every value in 'mirrorPrimes' is a prime and a reverse prime.
testMirrorPrimes :: Bool
testMirrorPrimes = all (\p -> (prime p) && (prime $ Lab1.reversal p)) mirrorPrimes


exercise4 :: IO ()
exercise4 = do
    putStrLn "--- Exercise 4 ---"
    putStrLn "All primes < 10000 that are also primes when reversed:\n"
    print mirrorPrimes
    putStrLn "\nWe can test whether every number generated by this function is\
    \ both a prime and a reversed prime:"
    print testMirrorPrimes
    putStrLn "\n"
