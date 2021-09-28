module Bonus3 where

-- EULER PROBLEM 191
-- A particular school offers cash rewards to children with good attendance
-- and punctuality. If they are absent for three consecutive days or late on
-- more than one occasion then they forfeit their prize.
--
-- During an n-day period a trinary string is formed for each child consisting
-- of L's (late), O's (on time), and A's (absent).
--
-- Although there are eighty-one trinary strings for a 4-day period that can be
-- formed, exactly forty-three strings would lead to a prize:
--
-- OOOO OOOA OOOL OOAO OOAA OOAL OOLO OOLA OAOO OAOA
-- OAOL OAAO OAAL OALO OALA OLOO OLOA OLAO OLAA AOOO
-- AOOA AOOL AOAO AOAA AOAL AOLO AOLA AAOO AAOA AAOL
-- AALO AALA ALOO ALOA ALAO ALAA LOOO LOOA LOAO LOAA
-- LAOO LAOA LAAO
--
-- How many "prize" strings exist over a 30-day period?


-- APPROACH
-- Our approach here is similar to Bonus2.hs. We again keep track of a histogram.
-- The criteria we keep track of this time is the total number of L's in the
-- string (either 0 or 1) and the number of A's at the end of the string (0, 1 or 2).
--
-- Let's denote a string having for example 1 L in total and 2 A's at the end as: L1A2
-- We now have 6 categories of strings: L0A0, L0A1, L0A2, L1A0, L1A1, L1A2. These can
-- be seen as the labels of our histogram.
--
-- If we have the histogram for n-letter strings, we can calculate the histogram
-- for (n+1)-letter strings using the following recurrence relation:
--
-- L0A0 = L0A0 + L0A1 + L0A2
-- L0A1 = L0A0
-- L0A2 = L0A1
--
-- L1A0 = L0A0 + L0A1 + L0A2 + L1A0 + L1A1 + L1A2
-- L1A1 = L1A0
-- L1A2 = L1A1
--
-- We can write this compactly as follows:


f :: ([Int],[Int]) -> ([Int],[Int])
f (xs,ys) = ((sum xs):(init xs), (sum $ xs++ys):(init ys))


bonus3 :: IO ()
bonus3 = do
    putStrLn "\n--- Bonus problem 191 ---\n"
    putStrLn "How many prize strings exist over a 30-day period?\n"

    print $ let (_,(n:_)) = iterate f ([1,1,0],[1,0,0]) !! 30 in n

    putStrLn "\nThe function could be tested by creating a bruteforce solution to the problem and then comparing the two functions for low input values."
