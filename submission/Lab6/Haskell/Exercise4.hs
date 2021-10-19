module Exercise4 where
import MultiplicationTable
import Exercise2
import Test.QuickCheck

-- Returns the percentage of mutants killed by a set of properties.
strength :: [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Integer
strength p f = do
    survivors <- (countSurvivors 10000 p f)
    return $ 100 - survivors `div` 100

exercise4 :: IO ()
exercise4 = do
    putStrLn "\n--- Exercise 4 ---\n"
    putStrLn "As an example, the percentage of mutants killed by properties 1 to 5 are:\n"
    percentage <- generate $ strength [prop_tenElements] multiplicationTable
    putStr (show percentage)
    putStrLn "%"
    percentage <- generate $ strength [prop_firstElementIsInput] multiplicationTable
    putStr (show percentage)
    putStrLn "%"
    percentage <- generate $ strength [prop_sumIsTriangleNumberTimesInput] multiplicationTable
    putStr (show percentage)
    putStrLn "%"
    percentage <- generate $ strength [prop_linear] multiplicationTable
    putStr (show percentage)
    putStrLn "%"
    percentage <- generate $ strength [prop_moduloIsZero] multiplicationTable
    putStr (show percentage)
    putStrLn "%"

    putStrLn "\nAnd the percentage of mutants killed by property 1 and property 5 together for example is:\n"
    percentage <- generate $ strength [prop_tenElements, prop_moduloIsZero] multiplicationTable
    putStr (show percentage)
    putStrLn "%"
