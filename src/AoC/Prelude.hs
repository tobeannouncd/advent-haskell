module AoC.Prelude
  ( todo
  , Solution, input, parseInput, answer
  , foldl'
  , maybeFail
  ) where

import GHC.Stack (HasCallStack)
import AoC.Solution ( Solution, answer, input, parseInput )
import Data.Foldable (foldl')

todo :: HasCallStack => String -> a
todo msg = error $ "TODO: " ++ msg

maybeFail :: MonadFail m => String -> Maybe a -> m a
maybeFail msg = maybe (fail msg) return