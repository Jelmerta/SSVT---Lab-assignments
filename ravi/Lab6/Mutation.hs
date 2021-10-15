module Mutation where
import Test.QuickCheck
import Data.List
import MultiplicationTable

mutate :: ([Integer] -> Integer -> Bool) -> (Integer -> [Integer]) -> Integer -> Gen Bool
mutate prop fut input = randomMutator >>= \mutator -> propertyExecutor prop (mutator $ fut input) input

randomMutator :: Gen ([Integer] -> Gen [Integer])
randomMutator = elements mutators

propertyExecutor :: ([Integer] -> Integer -> Bool) -> Gen [Integer] -> Integer -> Gen Bool
propertyExecutor prop o x = o >>= \output -> return $ prop output x

-- Mutators
mutators :: [[Integer] -> Gen [Integer]]
mutators = [addElements, removeElements, permute, add, multiply]

addElements :: [Integer] -> Gen [Integer]
addElements xs = do
  nums <- arbitrary :: Gen [Integer]
  num <- arbitrary :: Gen Integer
  return $ num : xs ++ nums

removeElements :: [Integer] -> Gen [Integer]
removeElements xs = choose (0, length xs - 1) >>= \x -> return $ take x xs

permute :: [Integer] -> Gen [Integer]
permute xs = do
    xs2 <- shuffle xs
    if xs2 /= xs then return xs2 else permute xs

add :: [Integer] -> Gen [Integer]
add xs = do
    n <- arbitrary :: Gen Integer
    let xs2 = (+n) <$> xs
    if xs2 /= xs then return xs2 else add xs

multiply :: [Integer] -> Gen [Integer]
multiply xs = do
    n <- arbitrary :: Gen Integer
    let xs2 = (*n) <$> xs
    if xs2 /= xs then return xs2 else multiply xs
