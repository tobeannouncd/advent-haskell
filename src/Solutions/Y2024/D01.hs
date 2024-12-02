module Solutions.Y2024.D01 (main) where

import AoC.Prelude
import Data.List (sort)

main :: Solution m => m ()
main = do
  (xs, ys) <- unzip . map parse . lines <$> input
  answer $ sum $ zipWith (\a b -> abs (a - b)) (sort xs) (sort ys)
  answer $ sum [x * count x ys | x <- xs]

count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

parse :: String -> (Int,Int)
parse xs =
  case map read $ words xs of
    [a,b] -> (a,b)
    _ -> error "no parse"