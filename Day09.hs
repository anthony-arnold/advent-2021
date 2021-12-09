module Main where
import Data.Char
import Control.Exception
import Data.Array
import Data.List
import qualified Data.Set as Set

readLines = do
    result <- try getLine :: IO (Either SomeException String)
    case result of
        Left _ -> return []
        Right line -> do
            rest <- readLines
            return (line : rest)


isLow c adj = all (>c) adj

risk c = 1 + digitToInt c

isInRange x y
  | x >= 0 = x < y
  | otherwise = False

adjacent i j w h = filter (\(x, y) -> (isInRange x w) && (isInRange y h)) points
  where points = [(i+1, j), (i-1, j), (i, j+1), (i, j-1)]

isPointLow grid i j w h = isLow (grid!(i, j)) compare
  where compare = foldl (\cs c -> (grid!c) : cs) [] adj
        adj = adjacent i j w h

step i j w h
  | i + 1 == w = (0, j + 1)
  | otherwise = (i + 1, j)

lowPoints grid i j w h
  | j == h = []
  | isPointLow grid i j w h = (i, j) : next
  | otherwise = next
  where next = lowPoints grid i1 j1 w h
        (i1, j1) = step i j w h

enc w (i, j) = i * w + j

foldfill grid w h (v, f) p = (nv, nf + f)
  where (nv, nf) = fill grid v w h p

fill grid v w h p
  | (grid!p) == '9' = (v, 0)
  | Set.member (enc w p) v = (v, 0)
  | otherwise = foldl (foldfill grid w h) (upd, 1) adj
  where upd = Set.insert (enc w p) v
        adj = adjacent i j w h
        (i, j) = p

basin grid w h p = size
  where (_, size) = fill grid empty w h p
        empty = Set.empty :: Set.Set Int

main = do
  -- Read input
  lines <- readLines

  -- Compute bounds
  let h = (length lines)
  let w = (length (head lines))
  let bounds = ((0, 0), (w-1, h-1))

  -- Fill grid
  let grid = array bounds [((i, j), (lines !! j) !! i) | i <- [0..w-1], j <- [0..h-1]]

  -- Find lowest points
  let points = lowPoints grid 0 0 w h

  -- Risk value
  let low = foldl (\r loc -> r + risk (grid!loc)) 0 points
  print low

  -- Find three largest basins
  let sizes = map (basin grid w h) points
  let largest = take 3 (reverse $ sort sizes)
  let m = foldl (*) 1 largest
  print m
