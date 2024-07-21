module Solutions.Y2015.D01 (main) where

import AoC.Prelude
import Data.List (elemIndex)

main :: Solution m => m ()
main = do
  inp <- mapM (maybeFail "invalid char" . change) =<< input
  let floors = scanl (+) 0 inp
  part2 <- maybe (fail "bad input") return (elemIndex (-1) floors)
  answer (last floors)
  answer part2

change :: Char -> Maybe Int
change '(' = Just 1
change ')' = Just (-1)
change _ = Nothing