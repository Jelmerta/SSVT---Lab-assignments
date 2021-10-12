module Exercise1 where
import LTS
import Data.List

-- Time spent:
-- 2.5 hours

-- Checks if list contains no duplicates
isSet :: Eq a => [a] -> Bool
isSet xs = nub xs == xs

-- Checks if a transition consists of elements from Q and L
validTransition :: [State] -> [Label] -> [Label] -> LabeledTransition -> Bool
validTransition q li lu (s1, l, s2) = (s1 `elem` q) && (s2 `elem` q) &&
    (l `elem` tau : li ++ lu)

-- Checks if the given IOLTS is valid
validateLTS :: IOLTS -> Bool
validateLTS (q, li, lu, t, q0) = isSet q && isSet li && isSet lu && isSet t &&
    q0 `elem` q && (tau `notElem` li ++ lu) && all (validTransition q li lu) t
    && null (li `intersect` lu)


exercise1 :: IO ()
exercise1 = do
    putStrLn "\n--- Exercise 1 ---\n"
    putStrLn "Factors that result in an invalid IOLTS but are still possible given the datatype are:\n"
    putStrLn "1. Q, LI, LU or T contain duplicate elements (then they aren't sets)"
    putStrLn "2. q0 is not in Q"
    putStrLn "3. LI ∪ LU contains τ"
    putStrLn "4. An element of T contains a state that is not in Q"
    putStrLn "5. An element of T contains a label that is not in LI ∪ LU ∪ τ"
    putStrLn "6. LI and LU intersect"

    -- Notes:
    -- The paper also states that Q, LI and LU should be countable. But since
    -- their datatypes are defined as a list of integers and strings, this
    -- already enforces that they are countable (given they contain no
    -- duplicates), since the list of all integers is countable and the list of
    -- all strings (of a finite alphabet) is also countable.
    -- This is why we believe it's sufficient to just list the first point that
    -- deals with duplicate elements.

    -- In practice, however, infinite lists will cause problems. For example
    -- ([1..], [], [], [], 1) is a valid LTS in theory, but our validateLTS
    -- function will not finish computation on this input.

    -- Additionally, the paper states that Q should not be empty. We did not
    -- make this a separate point, because point 2 already covers this.

    putStrLn "\nTesting minimal examples of invalid LTS's exhibiting the various factors:\n"
                                                            -- Factor:
    print $ validateLTS ([0,0],[],[],[],0)                      -- 1
    print $ validateLTS ([0],["a","a"],[],[],0)                 -- 1
    print $ validateLTS ([0],[],["a","a"],[],0)                 -- 1
    print $ validateLTS ([0],["a"],[],[(0,"a",0),(0,"a",0)],0)  -- 1
    print $ validateLTS ([],[],[],[],0)                         -- 2
    print $ validateLTS ([0],[tau],[],[],0)                     -- 3
    print $ validateLTS ([0],[],[tau],[],0)                     -- 3
    print $ validateLTS ([0],["a"],[],[(0,"a",1)],0)            -- 4
    print $ validateLTS ([0],[],[],[(0,"a",0)],0)               -- 5
    print $ validateLTS ([0],["a"],["a"],[],0)                  -- 6

    putStrLn "\nTesting one valid LTS:\n"

    print $ validateLTS ([7,3,1,8],["foo"],["bar"],[(3,"foo",1),(1,tau,7)],3)
