module Solutions (solutions) where

import Data.IntMap (fromList, IntMap)
import AoC.Solution (S)

import Solutions.Y2015 qualified as Y2015
import Solutions.Y2024 qualified as Y2024

solutions :: IntMap (IntMap S)
solutions =
  fromList
    [ (2015, Y2015.solutions)
    , (2024, Y2024.solutions)]