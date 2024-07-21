module Solutions (solutions) where

import Data.IntMap (fromList, IntMap)
import AoC.Solution (S)

import Solutions.Y2015 qualified as Y2015

solutions :: IntMap (IntMap S)
solutions =
  fromList
    [ (2015, Y2015.solutions) ]