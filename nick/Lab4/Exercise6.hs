-- Test approach symClos:
-- We need to test if the result is the symmetric closure of the relation
-- given to symClos.
-- Properties of the result of a symmetric closure that can be used to test
-- symClos are:
-- - All elements of the given relation are also in the resulting symmetric
--   closure, this also tests the weaker property that the size of the
--   symmetric closure must be >= the size of the given relation.
-- - The reverse of every relation must be in the resulting symmetric closure.
--   This means that for every relation (a,b), (b,a) must be in the resulting
--   symmetric closure. This also tests the weaker property that the size of
--   the symmetric closure must be 2 times the size of the given relation,
--   except for a relation of size 0.

-- Test approach trClos:
-- We need to test if the result is the transitive closure of the relation
-- given to trClos.
-- Properties of the result of a transitive closure that can be used to test
-- trClos are:
-- - All elements of the given relation are also in the resulting transitive
--   closure, this also tests the weaker property that the size of the
--   transitive closure must be >= the size of the given relation.
-- - If there are relations (a,b) and (b,c) in the given relation, (a,c)
--   must be in the resulting transitive closure. This means every possible
--   shortcut made up of the given relation must be in the transitive closure.

module Exercise6 where
    import SetOrd
    import Exercise3
    import Exercise5

    -- Test if all elements of the given relation are also in the resulting
    -- symmetric closure. This is done by testing if the given relation is a
    -- subset of the resulting symmetric closure.
    symClosPropSameElements :: Ord a => Rel a -> Bool
    symClosPropSameElements r = subSet (list2set r) (list2set (symClos r))

    -- Reverse the elements of a tuple.
    reverseTuple :: (a,a) -> (a,a)
    reverseTuple (a,b) = (b,a)

    -- Test if the reverse of every relation is in the resulting symmetric
    -- closure. This is done by reversing all tuples in the given relation
    -- and testing if it is a subset of the resultign symmetric closure.
    symClosPropReverse :: Ord a => Rel a -> Bool
    symClosPropReverse r = subSet (list2set (map reverseTuple r)) (list2set (symClos r))

    -- Test if all elements of the given relation are also in the resulting
    -- transitive closure. This is done by testing if the given relation is a
    -- subset of the resulting transitive closure.
    trClosPropSameElements :: Ord a => Rel a -> Bool
    trClosPropSameElements r = subSet (list2set r) (list2set (trClos r))

    -- Test if every possible shortcut made up of the given relation are in the
    -- transitive closure. This is done by testing if the set of shortcuts is a
    -- subset of the resylting transitive closure.
    trClosPropShortcuts :: Ord a => Rel a -> Bool
    trClosPropShortcuts r = subSet (list2set (r @@ r)) (list2set (trClos r))
