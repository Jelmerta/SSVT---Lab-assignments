module Exercise2 where
import Lecture3

-- The parser in Lecture3.hs takes a string as input, and if the string
-- represents a valid propositional formula, it turns it into a tree-like
-- structure called a Form. Lecture3.hs also contains a show function that
-- turns the Form back into a string. If the parser works correctly, we expect
-- this string to be the same as the input string.

-- To test the parser we will generate a bunch of valid formulas, turn them
-- into Forms with the parse function, and check if they're still the same
-- when turned back into strings.

-- Instead of generating formulas randomly, we will generate all valid formulas
-- obeying certain size constraints. Specifically these constraints are the
-- maximum nesting depth and the maximum number of unique variables. The
-- function allForms generates these formulas.

allForms :: Integer -> Integer -> [String]
allForms 1 vars = map show [1..vars]
allForms depth vars = ["*", "+", "-", "==>", "<=>"] >>= forms depth vars

forms :: Integer -> Integer -> String -> [String]
forms depth vars op
    | op == "-" = [op ++ f | f <- fs]
    | elem op ["*","+"] = [op ++ "(" ++ f1 ++ " " ++ f2 ++ ")" | f1 <- fs, f2 <- fs]
    | elem op ["==>","<=>"] = ["(" ++ f1 ++ op ++ f2 ++ ")" | f1 <- fs, f2 <- fs]
    where fs = allForms (depth - 1) vars


exercise2 :: IO ()
exercise2 = do
    putStrLn "--- Exercise 2 ---\n"
    putStrLn "Here is an example of a formula generated by allForms, when \
    \called with depth = 3 and vars = 4:\n"
    print $ (allForms 3 4)!!7000
    putStrLn "\nNow performing 18564 tests of all formulas with depth = 3 and vars = 4:\n"
    print $ all (\f -> (show $ head $ parse f) == f) (allForms 3 4)
