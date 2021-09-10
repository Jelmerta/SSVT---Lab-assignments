module Exercise6 where
import Lab1

primeProducts :: [Integer]
primeProducts = map (+1) (scanl1 (*) primes)

counterExample :: Integer
counterExample = head $ filter (\n -> not (prime n)) primeProducts

exercise6 :: IO ()
exercise6 = do
    putStrLn "--- Exercise 6 ---"
    print counterExample
    putStrLn ""
