module Solutions.Y2024.D02 (main) where
import AoC.Prelude
import Data.List (tails, inits)

main :: Solution m => m ()
main = do
  xss <- map (map read . words) . lines <$> input
  answer $ length $ filter (isSafeWith pure) xss
  answer $ length $ filter (isSafeWith holes) xss

isSafeWith :: ([Int] -> [[Int]]) -> [Int] -> Bool
isSafeWith f = any isSafe . f

holes :: [a] -> [[a]]
holes xs = xs : [i ++ t | (i, _:t) <- zip (inits xs) (tails xs)]

isSafe :: (Ord a, Num a) => [a] -> Bool
isSafe xs =
  case zipWith (-) xs (tail xs) of
    ys@(y:_)
      | y > 0 -> all (\x -> 1 <= x && x <= 3) ys
      | y < 0 -> all (\x -> -3 <= x && x <= -1) ys
    _ -> False
