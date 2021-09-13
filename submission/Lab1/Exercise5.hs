module Exercise5 where
import Lab1

-- Infinite list of the sums of every consecutive streak of 101 primes.
conseqSums :: [Integer] -> [Integer]
conseqSums a = sum (take 101 a) : conseqSums (tail a)

-- Finds smallest prime that is the sum of 101 consecutive primes.
smallestPrimeSum :: Integer
smallestPrimeSum = head $ filter prime (conseqSums primes)

-- First we check if the number produced by smallestPrimeSum is a prime
testPrime :: Integer -> Bool
testPrime = prime


-- Testing ideas: 
-- Test that the number produced is the sum of 101 consecutive primes
-- Going backwards
-- Hypothesis to be disproven: There should be at least one prime, produced by the sum of 101 consecutive primes, that is smaller than the one the software produces
-- Perfom test to check all the sums of 101 consecutive primes, where every prime is smaller than the one produced by the previous algorithm

exercise5 :: IO ()
exercise5 = do
    putStrLn "--- Exercise 5 ---\n"
    putStrLn "The smallest prime that is the sum of 101 consecutive primes is:"
    print smallestPrimeSum
    putStrLn "\nTesting if the found number is a prime:"
    print (testPrime smallestPrimeSum)
    putStrLn "\n"
