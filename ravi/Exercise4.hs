module Exercise4 where
import Lab1

mirrorPrimes :: [Integer]
mirrorPrimes = [n | n <- filter prime [1..10000], prime $ Lab1.reversal n]

exercise4 :: IO ()
exercise4 = do
    putStrLn "--- Exercise 4 ---"
    print mirrorPrimes
    putStrLn ""
