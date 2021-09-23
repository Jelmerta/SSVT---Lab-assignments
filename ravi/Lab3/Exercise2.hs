import Lecture3


forms :: Integer -> Integer -> String -> [String]
forms depth vars op
    | op == "-" = [op ++ f | f <- fs]
    | elem op ["*","+"] = [op ++ "(" ++ f1 ++ " " ++ f2 ++ ")" | f1 <- fs, f2 <- fs]
    | elem op ["==>","<=>"] = ["(" ++ f1 ++ op ++ f2 ++ ")" | f1 <- fs, f2 <- fs]
    where fs = allForms (depth - 1) vars

allForms :: Integer -> Integer -> [String]
allForms 1 vars = map show [1..vars]
allForms depth vars = ["*", "+", "-", "==>", "<=>"] >>= forms depth vars
