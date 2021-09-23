module Exercise2 where

import Lecture3
import Test.QuickCheck

instance Arbitrary Form where
    arbitrary = do
        p <- Prop Name
        p <- Neg Form
        return p