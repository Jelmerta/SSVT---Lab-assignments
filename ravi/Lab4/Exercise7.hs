module Exercise7 where

-- Time spent: 30 minutes

-- Exercise 7
-- Is there a difference between the symmetric closure of the transitive closure
-- of a relation R and the transitive closure of the symmetric closure of R?

-- Yes, there is a difference between sym(tr(R)) and tr(sym(R)). We will show
-- this with an example:

-- Let R = [(0,1),(2,1)]

-- Then:

-- tr(R) = [(0,1),(2,1)]
-- sym(tr(R)) = [(0,1),(1,0),(1,2),(2,1)]

-- sym(R) = [(0,1),(1,0),(1,2),(2,1)]
-- tr(sym(R)) = [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2),(2,0),(2,1),(2,2)]

-- So sym(tr(R)) and tr(sym(R)) are not the same thing.
