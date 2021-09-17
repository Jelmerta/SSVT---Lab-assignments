-- Time spent: 3 hours
module Exercise3 where
    
-- Testing properties strength
-- In this exercise we will compare the strength of different properties.
-- To verify whether a property q is stronger than property p on some domain S,
-- we have to check for every element e in domain S that if q(e) holds, then p(e) should also hold.
-- If it is also the case that when p(e) holds then q(e) holds, then the two properties are equivalent.

----- Helper functions begin -----
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

-- Provided functions for test properties
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

-- To compare properties, we can make use of the following code, which will tell us if a property is stronger, weaker or equivalent
    compar :: [a] -> (a -> Bool) -> (a -> Bool) -> String
    compar xs p q = let pq = stronger xs p q
                        qp = stronger xs q p
                    in
                        if pq && qp then "equivalent"
                        else if pq  then "stronger"
                        else if qp  then "weaker"
                        else             "incomparable"
----- Helper function end -----

                    -- a) Implement all properties from the Exercise 3 from Workshop 2
--    as Haskell functions of type Int -> Bool.
--    Consider a small domain like [(−10)..10].

-- Property 1
evenAndGreaterThan3 :: Integer -> Bool
evenAndGreaterThan3 n = even n && (n > 3)

-- Property 2
-- The even property is part of the Prelude library. It could simply be implemented as x `mod` 2 == 0

-- Property 3
evenOrGreaterThan3 :: Integer -> Bool
evenOrGreaterThan3 n = even n || (n > 3)

-- Property 4: This has the same effect as simply checking even, but just to be very explicit:
evenAndGreaterThan3OrEven :: Integer -> Bool
evenAndGreaterThan3OrEven n = evenAndGreaterThan3 n || even n

exercise3 :: IO ()
exercise3 = do
    -- b) Provide a descending strength list of all the implemented properties.
    putStrLn "\n--- Exercise 3 ---\n\n"
    putStrLn "W2EX3.1: Comparison of the function evenAndGreaterThan3 (even n && (n > 3)) with the function even in the range of [-10..10] tells us that evenAndGreaterThan3 is:"
    print (compar [-10..10] evenAndGreaterThan3 even)
    putStrLn "We now know that order of strength for the properties is [evenAndGreaterThan3, even]"
    putStrLn ""

    putStrLn "W2EX3.2: Comparison of the function evenOrGreaterOr3 (even n || (n > 3)) with the function even in the range of [-10..10] tells us that evenOrGreaterThan3 is:"
    print (compar [-10..10] evenOrGreaterThan3 even)
    putStrLn "We now know that order of strength for the properties is [evenAndGreaterThan3, even, evenOrGreaterOr3]"
    putStrLn ""

    putStrLn "W2EX3.3: Comparison of the function evenAndGreaterThan3OrEven (evenAndGreaterThan3 n || even n) with the function even in the range of [-10..10] tells us that evenAndGreaterThan3OrEven is:"
    print (compar [-10..10] evenAndGreaterThan3OrEven even)
    putStrLn "We now know that order of strength for the properties is [evenAndGreaterThan3, even, evenAndGreaterThan3OrEven, evenOrGreaterOr3], evenAndGreaterThan3OrEven and even being equivalent"
    putStrLn ""

    putStrLn "W2EX3.4: Comparison of the function even with the function evenAndGreaterThan3OrEven (evenAndGreaterThan3 n || even n) in the range of [-10..10] tells us that even is:"
    print (compar [-10..10] even evenAndGreaterThan3OrEven)
    putStrLn "We now know that order of strength for the properties is [evenAndGreaterThan3, even, evenAndGreaterThan3OrEven, evenOrGreaterOr3], evenAndGreaterThan3OrEven and even being equivalent"
    putStrLn ""