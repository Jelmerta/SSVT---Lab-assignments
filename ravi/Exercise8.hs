module Exercise8 where
import Lab1

-- As a first step we need to forget about who did it and focus on who is telling the truth or not
-- Thinking of the solution:
-- 1. I realised that Carl and Arnold cannot be on the same team (truth tellers or liars)
-- 2. I realised that Arnold and Jack can be on the same team only if they are both lying (which would imply that both Mathew and Peter are telling the truth)
-- 3. If Jack is telling the truth, it means that Mathew and Peter are lying so the rest of them are telling the truth to fullfil the 2:3 ratio provides by the teacher
-- 4. This is impossible from 1.,2. therefore we know that Jack is lying
-- 5. If Arnold is telling the truth Mathew or Peter is the second liar
-- 6. If 5 is true, given 4. it arises that Arnold and Carl are both truth tellers which is impossible
-- 7. Therefore, Arnold and Jack are lying as stated in 2.
-- 8. The killer is Jack which derives from the truth tellers Mathew and Peter
-- Be
-- To do so, we need to calculate all the possible combinations of three people telling the truth
-- There are 5 people so all the possible permutation are factorial 5 = 120 
-- We need to filter out the duplicates 
-- By the help of a truth table we need to determine the implications of

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
