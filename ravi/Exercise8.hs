module Exercise8 where
import Lab1

-- Determines whether a boy is telling the truth, assuming perp is guilty.
truthful :: Boy -> Boy -> Bool
truthful perp boy
    | boy == Matthew = perp /= Carl && perp /= Matthew
    | boy == Peter = perp == Matthew || perp == Jack
    | boy == Jack = (not $ truthful Matthew perp) && (not $ truthful Peter perp)
    | boy == Arnold = truthful Matthew perp /= truthful Peter perp
    | boy == Carl = not (truthful Arnold perp)

-- Gives a list indicating which boys are telling the truth, assuming perp is guilty.
truthVals :: Boy -> [Bool]
truthVals perp = map (truthful perp) boys

-- Counts the number of boys telling the truth, assuming perp is guilty.
nrTruthful :: Boy -> Int
nrTruthful perp = length $ filter (==True) (truthVals perp)

-- Returns the guilty boy and the list of boys that told the truth.
guilty, honest :: [Boy]
guilty = filter ((==3) . nrTruthful) boys
honest = filter (truthful $ head guilty) boys


exercise8 :: IO ()
exercise8 = do
    putStrLn "--- Exercise 8 ---"
    putStrLn "Guilty boy:"
    print guilty
    putStrLn "\nHonest boys:"
    print honest
    putStrLn "\n"
