module Exercise7 where
import Data.Char

double :: [Int] -> [Int]
double [] = []
double a = (2 * head a) : (dontdouble $ tail a)

dontdouble :: [Int] -> [Int]
dontdouble [] = []
dontdouble a = (head a) : (double $ tail a)

-- https://stackoverflow.com/a/3964069
digits :: Int -> [Int]
digits = map digitToInt . show

checkSum :: [Int] -> Int
checkSum a = sum $ map (sum . digits) (dontdouble a)

checkDigit :: [Int] -> Int
checkDigit a = 10 - (checkSum a) `mod` 10

luhn :: Int -> Bool
luhn n = let d = digits n in last d == (checkDigit $ init $ d)

exercise7 :: IO ()
exercise7 = do
    putStrLn "--- Exercise 7 ---"
    print (luhn 79927398713)
    putStrLn "\n"
