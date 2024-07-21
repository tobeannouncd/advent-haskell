module Solutions.Y2015.D02 (main) where

import AoC.Prelude
import AoC.Parsing
import Data.List (sort)

main :: Solution m => m ()
main = do
  boxes <- parseInput (sepEndBy1 boxP newline)
  answer (sum $ map paper boxes)
  answer (sum $ map ribbon boxes)

paper :: Num a => (a, a, a) -> a
paper (a,b,c) = 2*(a*b + a*c + b*c) + a*b
ribbon :: Num a => (a, a, a) -> a
ribbon (a,b,c) = 2*(a+b) + a*b*c


boxP :: Parser (Int,Int,Int)
boxP = do [a,b,c] <- sort <$> sepBy1 nat (char 'x')
          return (a,b,c)