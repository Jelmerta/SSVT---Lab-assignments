import Test.QuickCheck

func2 :: Integer -> Integer
func2 n = n

func2' :: Integer -> Integer
func2' n = n

prop_func2Equals :: Integer -> Bool
prop_func2Equals n = func2 n == func2' n

main :: IO ()
main = do
    quickCheck prop_func2Equals
    putStrLn "tests"