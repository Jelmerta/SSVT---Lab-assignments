-- Testing properties strength

import Lab1

-- Provided functions for test properties
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- a) Implement all properties from the Exercise 3 from Workshop 2
--    as Haskell functions of type Int -> Bool.
--    Consider a small domain like [(−10)..10].

evenAndGreaterThan3 :: Integer -> Bool
evenAndGreaterThan3 = \ x -> even x && x > 3 -- even n && (n > 3) -- \ x -> even x && x > 3

-- The even property is part of the Prelude library. It could simply be implemented as x `mod` 2 == 0

evenOrGreaterThan3 :: Integer -> Bool
evenOrGreaterThan3 n = even n || (n > 3)

-- This has the same effect as simply checking even, but just to be very explicit:
evenAndGreaterThan3OrEven :: Integer -> Bool
evenAndGreaterThan3OrEven n = evenAndGreaterThan3 n || even n


-- stronger [-10..10] even evenAndGreaterThan3
-- stronger [-10..10] evenOrGreaterThan3 evenAndGreaterThan3

-- findStrengthOrder :: [a] [a -> Bool] -> [a -> Bool]
-- findStrengthOrder xs (p:props) = stronger xs p q : findStrengthOfindSt


-- stronger [10..10] evenOrGreaterThan3 even

-- To compare properties, we can make use of the following code, which will tell us if a property is stronger, weaker or equivalent
compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
compar xs p q = let pq = stronger xs p q
                    qp = stronger xs q p
                in
                  if pq && qp then "equivalent"
                  else if pq  then "stronger"
                  else if qp  then "weaker"
                  else             "incomparable"




-- Testing and printing
exercise3 :: IO ()
exercise3 = do
    putStrLn "\n--- Exercise 3 ---\n"
    putStrLn "W2EX3: Comparing the function evenAndGreaterThan3 (\\ x -> even x && x > 3) with the function even gives using the function stronger in the range of [-10..10] gives us:"
    compar [0..10] even (even Lecture2..&&. (>3))
    compar [0..10] even (even Lecture2..||. (>3))

    print (stronger [-10..10] evenAndGreaterThan3 even)
    putStrLn "which means that the order of property strength is now:"
    putStrLn "[evenAndGreaterThan3, even]"

    putStrLn "Next, we can compare"

    -- (\ x -> even x && x > 3) or even
    -- \ = n(n+1)(2*n+1)/6 for natural numbers n:"
    -- quickCheckResult $ forAll genPositiveIntegers testSumSquares
    -- putStrLn "\nW1EX3: Using Quickcheck to test the property 1^3 + 2^3 + ... + n^3\
    -- \ = (n(n+1)/2)^2 for natural numbers n:"
    -- quickCheckResult $ forAll genPositiveIntegers testSumCubes
    -- putStrLn "\n"