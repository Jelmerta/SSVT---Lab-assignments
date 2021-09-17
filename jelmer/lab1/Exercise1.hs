import Test.QuickCheck

-- TODO Add property all positive numbers
-- Prove by induction that it holds for all natural numbers that 1^2 + 2^2 + ... + n^2 = (n(n+1)(2n+1)) / 6

genPositiveIntegers :: Gen Integer
genPositiveIntegers = abs <$> (arbitrary :: Gen Integer) `suchThat` (>0)

sumOfSquares1 :: Integer -> Integer
sumOfSquares1 n = sum (map (^2) [1..n])

sumOfSquares2 :: Integer -> Integer
sumOfSquares2 n = (n * (n + 1) * (2*n + 1)) `div` 6

testSumOfSquares :: Integer -> Bool
testSumOfSquares n = sumOfSquares1 n == sumOfSquares2 n

-- Call this in exercise 1:
-- quickCheckResult $ forAll genPositiveIntegers testSumOfSquares

-- Prove by induction that it holds for all natural numbers n that 1^3 + 2^3 + ... + n^3 = ( (n(n+1)) / 2 )^2
sumOfThirdPowers1 :: Integer -> Integer
sumOfThirdPowers1 n = sum (map (^3) [1..n])

sumOfThirdPowers2 :: Integer -> Integer
sumOfThirdPowers2 n = ((n * (n+1)) `div` 2 ) ^ 2

testSumOfThirdPowers :: Integer -> Bool
testSumOfThirdPowers n = sumOfThirdPowers1 n == sumOfThirdPowers2 n

-- quickCheckResult $ forAll genPositiveIntegers testSumOfThirdPowers