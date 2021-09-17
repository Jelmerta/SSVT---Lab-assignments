-- Testing properties strength
-- In this exercise we will compare the strength of different properties.
-- To verify whether a property q is stronger than property p on some domain S,
-- we have to check for every element e in domain S that if q(e) holds, then p(e) should also hold.
-- If it is also the case that when p(e) holds then q(e) holds, then the two properties are equivalent.

import Lab2

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