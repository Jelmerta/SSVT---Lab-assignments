module Exercise2 where
import MultiplicationTable
import Mutation
import Test.QuickCheck

countSurvivors :: Integer -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> Gen Integer
countSurvivors 0 _ _ = return 0
countSurvivors n p f = do
    i <- (arbitrary :: Gen Integer) `suchThat` (/= 0)
    o <- randomMutator >>= \m -> m (f i)
    s <- countSurvivors (n-1) p f
    return $ if survives p i o then s+1 else s

survives :: [([Integer] -> Integer -> Bool)] -> Integer -> [Integer] -> Bool
survives (h:t) i o = h o i && survives t i o
survives [] _ _ = True

properties :: [([Integer] -> Integer -> Bool)]
properties = [prop_tenElements, prop_firstElementIsInput,
    prop_sumIsTriangleNumberTimesInput, prop_linear, prop_moduloIsZero]

exercise2 :: IO ()
exercise2 = do
    putStrLn "\n--- Exercise 2 ---\n"
    survivors <- generate $ countSurvivors 10000 properties multiplicationTable
    putStrLn "Number of survivors:"
    print survivors
