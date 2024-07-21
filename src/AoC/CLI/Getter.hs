module AoC.CLI.Getter (getSolution) where

import Solutions (solutions)
import Data.IntMap ((!?))
import AoC.Solution (S)

getSolution :: Int -> Int -> IO S
getSolution year day = do
  y <- maybe (fail $ "No solutions for year" ++ show year)
             return
             (solutions !? year)
  maybe (fail $ "No solution for year " ++ show year ++ ", day " ++ show day)
        return
        (y !? day)