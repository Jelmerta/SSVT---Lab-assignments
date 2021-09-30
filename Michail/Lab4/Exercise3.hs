module Exercise3 where
-- Author: Michail Skalistiris Pikis
-- 20 mins


type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((x,y):rs) = (x,y) : (y,x) : symClos rs

exercise3 :: IO ()
exercise3 = do 
    putStrLn "\n--- Exercise 3 ---"
    putStrLn "\nsymClos [(1,2),(2,3),(3,4)] = "
    print (symClos [(1,2),(2,3),(3,4)])
    putStrLn ""