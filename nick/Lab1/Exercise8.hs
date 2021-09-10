-- stack --install-ghc runghc

import Lab1
import Test.QuickCheck

accuses :: Boy -> Boy -> Bool
accuses accuser accused = accuser == Peter && accused == Matthew
                          || accuser == Peter && accused == Jack
                          || accuser == Jack && accused == Matthew
                          || accuser == Jack && accused == Peter
                          || accuser == Carl && accused == Arnold

accusers :: Boy -> [Boy]
accusers = [x | x <- boys,  accuses x Boy]

guilty, honest :: [Boy]
