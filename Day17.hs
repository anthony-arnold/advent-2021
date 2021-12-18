module Main where
import Data.List
import qualified Data.Set as Set

srange s = (x1, x2)
  where x1 = (read s1 :: Int)
        x2 = (read s2 :: Int)
        s1 = takeWhile (/= '.') s
        s2 = takeWhile (/= ',') (drop (2 + length s1) s)

target str = (x, y)
  where x = srange $ drop 2 (w!!2)
        y = srange $ drop 2 (w!!3)
        w = words str

traj target (x, y) (x', y')
  | x > x2 || y < y1 = []
  | otherwise = (x,y):rest
  where rest = traj target (x + x', y + y') (max 0 (x' - 1), y' - 1)
        ((_, x2), (y1, _)) = target

between mn mx x = mn <= x && x <= mx
hit ((x1, x2), (y1, y2)) (x, y) = between x1 x2 x && between y1 y2 y
hits target steps = any (hit target) steps

shoot target x' y'
  | y' > 10 * (snd $ fst target) = []
  | x' > (snd $ fst target) =  shoot target 1 (1 + y')
  | hits target steps = ((x', y'), maximum $ map snd $ steps) : rest
  | otherwise = rest
  where steps = traj target (0, 0) (x', y')
        rest = shoot target (1 + x') y'

shootat target = shoot target 1 (-10 * (snd $ fst target))

main = do
  str <- getLine
  let launches = shootat (target str)
  print $ maximum $ map snd $ launches
  let distinct = Set.fromList $ map fst $ launches
  print $ Set.size distinct
