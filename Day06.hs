module Main where
import qualified Data.Map as Map

toList :: String -> [Int]
toList "" = []
toList s = digit : toList (drop 2 s)
  where digit = read (take 1 s) :: Int

fish :: Int -> Map.Map Int Int -> (Int, Map.Map Int Int)
fish r m
  | r <= 0 = (0, m)
  | Map.member r m = (Map.findWithDefault 0 r m, m)
  | otherwise = (val, mapped)
  where mapped = Map.insert r val nextMap
        val = 1 + nextFish + childFish
        (nextFish, nextMap) = fish next childMap
        (childFish, childMap) = fish child m
        next = r - 7
        child = r - 9

foldFish :: Int -> (Map.Map Int Int, Int) -> Int -> (Map.Map Int Int, Int)
foldFish start (mem, tot) val = (updated, total)
  where total = tot + f
        (f, updated) = fish (start - val) mem

ans :: Int -> [Int] -> Int
ans t x = total
  where
    (_, total) = foldl (foldFish t) (mem, length x) x
    mem =  Map.empty :: Map.Map Int Int

main = do
  s <- getLine
  let x = toList s
  print (ans 80 x)
  print (ans 256 x)
