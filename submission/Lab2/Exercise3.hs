-- Time spent: 3.5 hours
module Exercise3 where

import Data.List
    
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

-- Provided functions for testing property strength
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

-- All four properties have the same definition:
prop_evenAndGreaterThan3, prop_even, prop_evenOrGreaterThan3, prop_evenAndGreaterThan3OrEven :: Integer -> Bool
-- Implementations of the different properties:
prop_evenAndGreaterThan3 n = even n && (n > 3)
prop_even = even
prop_evenOrGreaterThan3 n = even n || (n > 3)
prop_evenAndGreaterThan3OrEven n = prop_evenAndGreaterThan3 n || even n -- This has the same effect as simply checking even

-- We can define a function that provides us with an ordering when two functions with the same definition are provided in the given domain
-- Note: properties may be incomparable
propertySort :: String -> String -> Ordering
propertySort prop1 prop2
    | compar [-10..10] (findPropFromName prop1) (findPropFromName prop2) == "stronger" = LT
    | otherwise = GT
    -- If compar of two properties is equal, it does not matter if GT or LT is reached, as the property will be on one side of the other property.
    -- This does not account for incomparable. It would probably be better if the property would not be added to the sorting, or perhaps just fail completely.
    -- This does not matter in this exercise as the properties are comparable here.

-- Sadly we can't simply print function names with type (Integer -> Bool), so we have to be clever and first find the right function to be able to sort the properties
-- To make sure we order on the property, this also has the effect of executing the function here
findPropFromName :: String -> (Integer -> Bool)
findPropFromName property
    | property == "prop_evenAndGreaterThan3" = prop_evenAndGreaterThan3
    | property == "prop_even" = prop_even
    | property == "prop_evenOrGreaterThan3" = prop_evenOrGreaterThan3
    | property == "prop_evenAndGreaterThan3OrEven" = prop_evenAndGreaterThan3OrEven

-- To sort all the properties, we simply sort all the defined properties using the propertySort function (after finding the actual definition of the property)
sortAllPropertiesByStrength :: [String] -> [String]
sortAllPropertiesByStrength = sortBy propertySort

exercise3 :: IO ()
exercise3 = do
    -- b) Provide a descending strength list of all the implemented properties.
    -- We do this by calling the provided functions above.
    putStrLn "\n--- Exercise 3 ---\n\n"
    putStrLn "Ordering the properties prop_evenAndGreaterThan3, prop_even, prop_evenOrGreaterThan3 and prop_evenAndGreaterThan3OrEven by strength\n"
    putStrLn "Strongest properties are shown first:\n"
    print (sortAllPropertiesByStrength ["prop_evenAndGreaterThan3", "prop_even", "prop_evenOrGreaterThan3", "prop_evenAndGreaterThan3OrEven"])
    putStrLn ""