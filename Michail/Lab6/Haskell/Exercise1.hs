module Exercise1 where

-- Potential stronger mutations
---- add x to each element of the output least
---- substract x from each element of the output list
---- Ranked mutators (weaker first)
------ create a list with ten random elements
------ order the output randomly (weaker)
------ move each element one place to the right (stronger)
------ empty set (strongest)

swapToRight :: [Integer] -> [Integer]
swapToRight xs =   xs !! (length xs - 1)  : init xs
------ empty set (strongest)


