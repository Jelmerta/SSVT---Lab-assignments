module Exercise1 where
import LTS
import Data.List

isSet :: Eq a => [a] -> Bool
isSet xs = nub xs == xs

validTransition :: [State] -> [Label] -> LabeledTransition -> Bool
validTransition q l (s1, l1, s2) = (s1 `elem` q) && (s2 `elem` q) &&
    (l1 `elem` l || l1 == tau)

validateLTS :: LTS -> Bool
validateLTS (q, l, t, q0) = isSet q && isSet l && isSet t &&
    (not $ tau `elem` l) && all (validTransition q l) t && q0 `elem` q


exercise1 :: IO ()
exercise1 = do
    putStrLn "\n--- Exercise 1 ---\n"
    putStrLn "Factors that result in an invalid LTS but are still possible given the datatype are:\n"
    putStrLn "1. Q, L or T contain duplicate elements (then they aren't sets)"
    putStrLn "2. q0 is not in Q"
    putStrLn "3. L contains the label 'tau'"
    putStrLn "4. An element of T contains a state that is not in Q"
    putStrLn "5. An element of T contains a label that is not in L and not equal to 'tau'"

    -- Notes:
    -- The paper also states that Q and L should be countable. But since
    -- their datatypes are defined as a list of integers and strings, this
    -- already enforces that they are countable (given they contain no
    -- duplicates), since the list of all integers is countable and the list of
    -- all strings (of a finite alphabet) is also countable.
    -- This is why we believe it's sufficient, in theory, to just list the
    -- first point that deals with duplicate elements.

    -- In practice, however, infinite lists will cause problems. For example
    -- ([1..], [], [], 1) is a valid LTS, but our validateLTS function will not
    -- finish computation on this input.

    -- Additionally, the paper states that Q should not be empty. We did not
    -- make this a separate point, because point 2 already covers this.

    putStrLn "\nTesting minimal examples of invalid LTS's exhibiting the various factors:\n"
                                                            -- Factor:
    print $ validateLTS ([0,0],[],[],0)                         -- 1
    print $ validateLTS ([0],["a","a"],[],0)                    -- 1
    print $ validateLTS ([0],["a"],[(0,"a",0),(0,"a",0)],0)     -- 1
    print $ validateLTS ([],[],[],0)                            -- 2
    print $ validateLTS ([0],[tau],[],0)                        -- 3
    print $ validateLTS ([0],["a"],[(0,"a",1)],0)               -- 4
    print $ validateLTS ([0],[],[(0,"a",0)],0)                  -- 5

    putStrLn "\nTesting one valid LTS:\n"

    print $ validateLTS ([7,3,1,8],["foo","bar"],[(3,"foo",1),(1,tau,7)],3)
