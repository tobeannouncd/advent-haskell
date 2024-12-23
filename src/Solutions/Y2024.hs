module Solutions.Y2024 (solutions) where

import Data.IntMap (IntMap, fromList)
import Solutions.Y2024.D01 qualified as D01

import AoC.Solution(S(..))
import qualified Solutions.Y2024.D02 as D02

solutions :: IntMap S
solutions =
  fromList
    [ (1, S D01.main)
    , (2, S D02.main)]
