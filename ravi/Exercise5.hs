module Exercise5 where
import Lab1

-- Infinite list of the sums of every consecutive streak of 101 primes.
conseqSums :: [Integer] -> [Integer]
conseqSums a = sum (take 101 a) : conseqSums (tail a)

-- Finds smallest prime that is the sum of 101 consecutive primes.
smallestPrimeSum :: Integer
smallestPrimeSum = head $ filter prime (conseqSums primes)

exercise5 :: IO ()
exercise5 = do
    putStrLn "--- Exercise 5 ---"
    putStrLn "The smallest prime that is the sum of 101 consecutive primes is:"
    print smallestPrimeSum
    putStrLn "\n"
