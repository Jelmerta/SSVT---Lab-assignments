module Exercise1 where

exercise1 :: IO ()
exercise1 = do
    putStrLn "\n--- Exercise 1 ---\n"
    putStrLn "Mutators added to Mutation.hs:\n"
    putStrLn "- permute:    randomly permutes the list of numbers"
    putStrLn "- add:        add a random constant to all numbers in the list"
    putStrLn "- multiply:   multiply all the numbers in the list by a random constant"
    putStrLn "\nThese mutators might be interesting because they preserve different properties."
