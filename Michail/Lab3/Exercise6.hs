module Exercise6 where

import Data.List
import Lecture3

type Clause  = [Int]
type Clauses = [Clause]

-- CNF to CLS parser 
-- Preconditions: 
---- Form in CNF
cnf2cls :: Form -> Clauses
cnf2cls (Prop x) = [[x]]
cnf2cls (Neg (Prop x)) = [[-x]]
cnf2cls (Dsj fs) = [concat (foldMap cnf2cls fs)]
cnf2cls (Cnj fs) = foldMap cnf2cls fs
form11 = Cnj[Dsj[Neg p,q],q]

draw = cnf2cls (Cnj [(Dsj [p, q]), (Dsj [Neg p, q]), (Dsj [q, Neg q])])

-- Postcondition:
---- The properties of CNF form should be the same as the properties after the function
---- 
exercise6 :: IO ()
exercise6 = do
    putStrLn "\n--- Exercise 6 ---\n"
    putStrLn "- CNF to CLS parser\n"
    putStrLn "CNF:\n"
    print form11
    putStrLn ""
    putStrLn "CLS:\n"
    print (cnf2cls form11)
    putStrLn ""

