module Exercise8 where
import Lab1

truthful :: Boy -> Boy -> Bool
truthful perp boy
    | boy == Matthew = perp /= Carl && perp /= Matthew
    | boy == Peter = perp == Matthew || perp == Jack
    | boy == Jack = (not $ truthful Matthew perp) && (not $ truthful Peter perp)
    | boy == Arnold = truthful Matthew perp /= truthful Peter perp
    | boy == Carl = not (truthful Arnold perp)

truthVals :: Boy -> [Bool]
truthVals perp = map (truthful perp) boys

nrTruthful :: Boy -> Int
nrTruthful perp = length $ filter (==True) (truthVals perp)

guilty, honest :: [Boy]
guilty = filter ((==3) . nrTruthful) boys
honest = filter (truthful $ head guilty) boys

exercise8 :: IO ()
exercise8 = do
    putStrLn "--- Exercise 8 ---"
    print guilty
    print honest
    putStrLn ""
