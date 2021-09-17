module Exercise4 where

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation [] a = null a
isPermutation (h:t) a = isPermutation t (filter (/= h) a)

exercise4 :: IO ()
exercise4 = do
    putStrLn "--- Exercise 4 ---\n"
    print $ isPermutation [3,5,8,2] [2,8,5,3]
