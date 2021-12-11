module Main where
import Data.Char
import Control.Exception
import Data.Array
import qualified Data.Set as Set

rng = [(i, j) | i <- [0..9], j <- [0..9]]

adjacent i j = filter (\(x, y) -> x /= i || y /= j) sibs
  where sibs = [(x, y) | x <- xs, y <- ys]
        xs = [max 0 (i-1) .. min 9 (i+1)]
        ys = [max 0 (j-1) .. min 9 (j+1)]

enc i j = j*10 + i

pwrd grid i j = grid!(i, j) > 9

again grid s = any (\(x, y) -> (unvisited x y) && (pwrd grid x y)) rng
  where unvisited x y = Set.notMember (enc x y) s

flash grid s 10 j = flash grid s 0 (1+j)
flash grid s _ 10
  | again grid s = flash grid s 0 0
  | otherwise = grid

flash grid s i j
  | not (pwrd grid i j) || Set.member (enc i j) s = flash grid s (1+i) j
  | otherwise = flash ngrid ns (1+i) j
    where ngrid = foldl (\ng (x, y) -> inc ng x y) grid adj
          ns = Set.insert (enc i j) s
          adj = adjacent i j

inc grid i j = grid // [((i, j), 1 + grid!(i, j))]

flashed grid = filter (\(i, j) -> grid!(i, j) > 9) rng

reset grid = grid // map (\idx -> (idx, 0)) (flashed grid)

report 100 num = do
  print num

report _ _ = return ()

run steps num grid = do
  report steps num
  if all (\(x, y) -> grid!(x,y)==0) rng
    then print steps
    else run (1+steps) (numFlash + num) next
  where next = reset flashing
        numFlash = length (flashed flashing)
        flashing = flash incremented empty 0 0
        empty = Set.empty :: Set.Set Int
        incremented = foldl (\g (x,y) -> inc g x y) grid rng

pwrs "" _ _ = []
pwrs (c:line) i j = ((i, j), (ord c) - (ord '0')) : pwrs line (i+1) j

getGrid j = do
  result <- try getLine :: IO (Either SomeException String)
  case result of
    Left _ -> return []
    Right line -> do
      rest <- getGrid (1 + j)
      return ((pwrs line 0 j) ++ rest)

readGrid = do
  values <- getGrid 0
  return (array ((0,0), (9,9)) values)


main = do
  grid <- readGrid
  run 0 0 grid
