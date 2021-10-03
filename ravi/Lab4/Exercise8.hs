module Exercise8 where
import Lecture4

-- Time spent: 2.5 hours

-- Show function defined for expressions
instance Show Expr where
    show (I n) = show n
    show (V var) = var
    show (Add expr1 expr2) = (show expr1) ++ " + " ++ (show expr2)
    show (Subtr expr1 expr2) = (show expr1) ++ " - " ++ (show expr2)
    show (Mult expr1 expr2) = (show expr1) ++ " * " ++ (show expr2)

-- Show function defined for conditions
instance Show Condition where
    show (Prp var) = var
    show (Eq expr1 expr2) = (show expr1) ++ " == " ++ (show expr2)
    show (Lt expr1 expr2) = (show expr1) ++ " < " ++ (show expr2)
    show (Gt expr1 expr2) = (show expr1) ++ " > " ++ (show expr2)
    show (Ng cond) = "not" ++ (show cond)
    show (Cj (h:t)) = (show h) ++ " and " ++ (show t)
    show (Dj (h:t)) = (show h) ++ " or " ++ (show t)

-- Show function defined for statements
instance Show Statement where
    show (Ass var expr) = var ++ " = " ++ (show expr)
    show (Cond cond stmt1 stmt2) = (show stmt1) ++ (show cond) ++ (show stmt2)
    show (Seq (h:t)) = (show h) ++ "\n" ++ (show (Seq t))
    show (Seq []) = ""
    show (While cond stmt) = "\nwhile (" ++ (show cond) ++ ") {\n" ++ (show stmt) ++ "}"


exercise8 :: IO ()
exercise8 = do
    putStrLn "\n--- Exercise 8 ---\n"
    putStrLn "As a demonstration of our show function, we will print the 'fib' \
              \algorithm defined in Lecture4.hs:\n"
    print fib
