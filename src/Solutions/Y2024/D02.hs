module Solutions.Y2024.D02 (main) where
import AoC.Prelude
import Data.List (tails, inits)

main :: Solution m => m ()
main = do
  xss <- map (map read . words) . lines <$> input
  answer $ length $ filter (isSafeWith pure) xss
  answer $ length $ filter (isSafeWith holes) xss
  -- answer $ length $ filter (isGood False) xss
  -- answer $ length $ filter (isGood True) xss

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

-- | @\(O(n)\)@ solution. Not really necessary due to the size of the given
--   inputs.
-- isGood :: Bool -> [Int] -> Bool
-- isGood p xs = good p xs || good p (reverse xs)

-- good :: Bool -> [Int] -> Bool
-- good p = go []
--   where
--     go _ [] = True
--     go [] (x:xs) = go [x] xs
--     go xs@(x:_) (y:ys)
--       | d <- x - y
--       , 1 <= d, d <= 3 = go (y:xs) ys
--     go [x] ys = p && any (good False) [ys, x:drop 1 ys]
--     go (x:x':_) ys = p && any (good False) [x':ys, x:drop 1 ys]