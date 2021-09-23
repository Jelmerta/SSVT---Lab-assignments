module Exercise5 where

-- Consider the following implementation of a function sub::Form->Set Form that finds 
-- all the sub-formulae of a given formula.
import SetOrd
import Lecture3


sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Dsj [f1,f2]) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

-- How can you prove that the sub implementation is correct? Test the implementation with two QuickCheck properties.

-- Preconditions
---- Valid Form input

-- Postconditions
---- All elements are included in the initial form
