module Solutions.Y2024.D02 (main) where
import AoC.Prelude
import Data.List (tails, inits)

main :: Solution m => m ()
main = do
  xss <- map (map read . words) . lines <$> input
  answer $ length $ filter (isSafeWith pure) xss
  answer $ length $ filter (isSafeWith holes) xss

isSafeWith :: ([Int] -> [[Int]]) -> [Int] -> Bool
isSafeWith f = any (isSafe . (zipWith (-) <*> tail)) . f

holes :: [a] -> [[a]]
holes xs = xs : [i ++ t | (i, _:t) <- zip (inits xs) (tails xs)]

isSafe :: (Ord a, Num a) => [a] -> Bool
isSafe xs@(x:_)
  | x < 0 = all (\y -> -3 <= y && y <= -1) xs
  | x > 0 = all (\y -> 1 <= y && y <= 3) xs
isSafe _ = False
