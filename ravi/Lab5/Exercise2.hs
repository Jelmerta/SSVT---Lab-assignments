module Exercise2 where
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
               return $ nub l

transitionGen :: [State] -> [Label] -> Gen LabeledTransition
transitionGen q l = do s1 <- elements q
                       s2 <- elements q
                       l1 <- elements $ tau:l
                       return (s1, l1, s2)

transitionsGen :: [State] -> [Label] -> Gen [LabeledTransition]
transitionsGen q l = do n <- choose (0,9)
                        transitions <- vectorOf n $ transitionGen q l
                        return $ nub transitions

ltsGen :: Gen IOLTS
ltsGen = do states <- statesGen
            inputs <- labelsGen
            outputs <- labelsGen
            transitions <- transitionsGen states $ inputs ++ outputs
            start <- elements states
            return (states, inputs, outputs, transitions, start)


exercise2 :: IO ()
exercise2 = do
    putStrLn "\n--- Exercise 2 ---\n"
    lts <- generate ltsGen
    print lts
