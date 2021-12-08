module Main where
import Data.List
import qualified Data.Map as Map

median x = s !! m
  where s = sort x
        m = (length x) `div` 2

mean x = (sum x) `div` (length x)

toList :: String -> [Int]
toList "" = []
toList s = num : toList (drop ((length digits) + 1) s)
  where num = read digits :: Int
        digits = takeWhile (/= ',') s

one x = foldl (\ fuel h -> fuel + abs (h - tgt)) 0 x
  where tgt = median x

summa n = n * (n + 1) `div` 2

test x a = foldl (\ fuel h -> fuel + (summa $ abs (h - a))) 0 x
two x = min (test x m) (test x (m+1))
  where m = mean x

main = do
  s <- getLine
  let x = toList s
  print (one x)
  print (two x)
