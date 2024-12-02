module Solutions.Y2015 (solutions) where

import Data.IntMap (IntMap, fromList)
import Solutions.Y2015.D01 qualified as D01
import AoC.Solution (S(..))
import qualified Solutions.Y2015.D02 as D02
import qualified Solutions.Y2015.D03 as D03

solutions :: IntMap S
solutions =
  fromList
    [ (1, S D01.main)
    , (2, S D02.main)
    , (3, S D03.main) ]