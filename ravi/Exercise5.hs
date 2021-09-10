module Exercise5 where
import Lab1

conseqSums :: [Integer] -> [Integer]
conseqSums a = sum (take 101 a) : conseqSums (tail a)

smallestPrimeSum :: Integer
smallestPrimeSum = head $ filter prime (conseqSums primes)

exercise5 :: IO ()
exercise5 = do
    putStrLn "--- Exercise 5 ---"
    print smallestPrimeSum
    putStrLn ""
