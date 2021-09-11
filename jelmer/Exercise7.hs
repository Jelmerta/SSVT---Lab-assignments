-- In this exercise we want to write the Luhn algorithm which is used to validate credit card numbers.
-- Implementations for major credit card companies are also provided.

-- TODO Optionally we can add automated tests here, but we would need to generate labelled data...? Not sure how

-- luhn :: Integer -> Bool
-- luhn n = [read i | i < show n]

convertStringToListOfIndividualNumbers :: String -> [Integer]
convertNumberToListOfIndividualNumbers [] = []
convertStringToListOfIndividualNumbers (s:ss) = digitToInt s : convertStringToListOfIndividualNumbers ss
