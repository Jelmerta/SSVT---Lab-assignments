-- stack --install-ghc runghc

import Lab1
import Data.List ( permutations )
import Test.QuickCheck

-- From http://progopedia.com/example/factorial/118/
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

lengthPermutations :: [Integer] -> Integer
lengthPermutations list = toInteger (length (permutations list))

lengthPermutations' :: [Integer] ->  Integer
lengthPermutations' list = factorial (toInteger (length list))

propLength :: Integer -> Bool
propLength n = lengthPermutations [1..n] == lengthPermutations' [1..n]

-- Property is hard to test because the generated list, and the number to take the factorial of become very long, so it takes a lot of time, therefore quickcheck is only performed 20 times.

main :: IO ()
main = do
    quickCheck (withMaxSuccess 20 propLength)