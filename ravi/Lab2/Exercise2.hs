module Exercise2 where
import System.Random
import Test.QuickCheck
import Data.List

-- HELPER FUNCTIONS --

-- Different triangle shapes
data Shape = NoTriangle | Equilateral | Isosceles
    | Rectangular | Other deriving (Eq,Show)

-- Get random integer between a and b.
getInt :: Int -> Int -> IO Int
getInt a b = getStdRandom (randomR (a,b))

-- General testing function:
-- k = current test, n = total tests, f = function to be tested
-- g = input generator, p = postcondition to be checked
test :: Int -> Int -> ((Int, Int, Int) -> Shape) -> (IO (Int, Int, Int)) ->
        (Shape -> Bool) -> IO ()
test k n f g p = if k == n then print (show n ++ " tests passed")
    else do
        t <- g
        if p (f t) then
            do print ("pass on: " ++ show t)
               test (k+1) n f g p
            else error ("failed test on: " ++ show t)


-- IMPLEMENTATION

-- NoTriangle:
-- https://en.wikipedia.org/wiki/Triangle_inequality#Converse
-- "The converse of the triangle inequality theorem is also true: if three real
-- numbers are such that each is less than the sum of the others, then there
-- exists a triangle with these numbers as its side lengths and with positive
-- area; and if one number equals the sum of the other two, there exists a
-- degenerate triangle (that is, with zero area) with these numbers as its side
-- lengths."
-- So a proper triangle exists when:
-- a + b > c, b + c > a, and c + a > b
-- This can be stated more succinctly as:
-- 2 max(a,b,c) < a + b + c
-- Which means a proper triangle doesn't exist when:
-- 2 max(a,b,c) >= a + b + c

-- Rectangular:
-- https://en.wikipedia.org/wiki/Pythagorean_theorem#Converse
-- "The converse of the theorem is also true: For any three positive numbers
-- a, b, c such that a^2 + b2 = c2, there exists a triangle with sides a, b, c
-- and every such triangle has a right angle between the sides of lengths a and b."

-- Equilateral and Isosceles:
-- If a triangle has 3 equal sides it's an equilateral triangle, while if it has
-- only 2 equal sides, it's an Isosceles triangle.

-- Given the sidelengths of a triangle, returns the shape of the triangle based
-- on the definitions above.
triangle :: (Int, Int, Int) -> Shape
triangle (a,b,c)
    | 2 * maximum([a,b,c]) >= a + b + c = NoTriangle
    | a^2 == b^2 + c^2 || b^2 == a^2 + c^2 || c^2 == a^2 + b^2 = Rectangular
    | n == 1 = Equilateral
    | n == 2 = Isosceles
    | otherwise = Other
    where n = length $ nub [a,b,c]


-- TESTING

-- It doesn't make much sense here to generate totally random input and then
-- check if precondition implies postcondition. Mainly because equilateral and
-- rectangular triangles are quite rare and would barely be encountered.
--
-- Therefore we will generate input satisfying the desired preconditions and
-- check if the postconditions hold on the output.

-- Rectangular:
-- https://en.wikipedia.org/wiki/Pythagorean_triple#Generating_a_triple
-- "Euclid's formula is a fundamental formula for generating Pythagorean triples
-- given an arbitrary pair of integers m and n with m > n > 0. The formula
-- states that the integers a = m^2 - n^2, b = 2mn, c = m^2 + n^2 form a
-- Pythagorean triple."
--
-- Given these triples as inputs we expect our function to return Rectangular.
-- We use the following generator:
genTriple :: IO (Int, Int, Int)
genTriple = do
    n <- getInt 1 100
    m <- getInt (n+1) 100
    return (m^2 - n^2, 2*m*n, m^2 + n^2)

-- Equilateral:
-- Given three equal numbers as input we expect our function to return Equilateral.
-- It might seem like we're reimplementing our solution here, but the original
-- implementation is quite different, so this test should still be useful.
-- We use the following generator:
gen3Equal :: IO (Int, Int, Int)
gen3Equal = do
    n <- getInt 1 100
    return (n, n, n)



exercise2 :: IO ()
exercise2 = do
    putStrLn "--- Exercise 2 ---\n"
    putStrLn "Rectangular test:"
    test 0 10 (triangle) (genTriple) (==Rectangular)
    putStrLn "\nEquilateral test:"
    test 0 10 (triangle) (gen3Equal) (==Equilateral)
