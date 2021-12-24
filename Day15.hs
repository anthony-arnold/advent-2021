module Main where
import Control.Exception
import Data.Array
import Data.Char
import SPFA

ints "" = []
ints (c:cs) = (ord c - ord '0') : (ints cs)

readLines = do
  r <- try getLine :: IO (Either SomeException String)
  case r of
    Left _ -> return []
    Right line -> do
      rest <- readLines
      return ((ints line):rest)

gridLine [] _ _ = []
gridLine (c:cs) x y = ((x, y), c):(gridLine cs (x+1) y)

gridRow [] _ = []
gridRow (l:ls) y = (gridLine l 0 y) ++ (gridRow ls (y+1))

toGrid lines = array ((0, 0), (w, h)) (gridRow lines 0)
  where w = length $ head lines
        h = length lines

adj w h p = filter (\(x, y) -> x >= 0 && y >= 0 && x < w && y < h) sibs
  where sibs = [(i+1,j),(i-1,j),(i,j+1),(i,j-1)]
        (i, j) = p

mag grid w h (x, y) = pol
  where pol
          | n' > 9 = n' - 9
          | otherwise = n'
        n' = n + dx + dy
        n = grid!(x', y')
        (dx, x') = divMod x w
        (dy, y') = divMod y h

answer grid m = case shortest next done (0, (0, 0)) of
                  Nothing -> maxBound :: Int
                  Just (len, _) -> len
  where
    done v = v == (w*m-1, h*m-1)
    next (dist, p) = map (\n -> (dist + xp n, n)) (adj (w*m) (h*m) p)
    xp n = mag grid w h n
    ((_, _), (w, h)) = bounds grid

main = do
  lines <- readLines
  let g = toGrid lines
  print $ answer g 1
  print $ answer g 5
