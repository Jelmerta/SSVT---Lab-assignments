module Exercise2 where
import Exercise1
import LTS
import Test.QuickCheck
import Data.List

stateGen :: Gen State
stateGen = choose (0,9)

statesGen :: Gen [State]
statesGen = do n <- choose (1,9)
               q <- vectorOf n stateGen
               return $ nub q

characterGen :: Gen Char
characterGen = elements ['a'..'z']

labelGen :: Gen Label
labelGen = vectorOf 3 characterGen

labelsGen :: Gen [Label]
labelsGen = do n <- choose (0,9)
               l <- vectorOf n labelGen
               return $ nub l \\ [tau]

transitionGen :: [State] -> [Label] -> Gen LabeledTransition
transitionGen q l = do s1 <- elements q
                       s2 <- elements q
                       l1 <- elements $ tau:l
                       return (s1, l1, s2)

transitionsGen :: [State] -> [Label] -> Gen [LabeledTransition]
transitionsGen q l = do n <- choose (0,9)
                        t <- vectorOf n $ transitionGen q l
                        return $ nub t

ltsGen :: Gen IOLTS
ltsGen = do q <- statesGen
            l1 <- labelsGen
            l2 <- labelsGen
            let li = l1 \\ l2
            let lu = l2 \\ l1
            t <- transitionsGen q $ li ++ lu
            q0 <- elements q
            return (q, li, lu, t, q0)


exercise2 :: IO ()
exercise2 = do
    putStrLn "\n--- Exercise 2 ---\n"
    putStrLn "Example of a generated IOLTS:\n"
    lts <- generate ltsGen
    print lts
    putStrLn "\nTesting the validate function:\n"
    quickCheck $ forAll ltsGen validateLTS
    putStrLn ""
