module Main where
import Control.Exception
import Data.Bits

line :: String -> [Int]
line "" = []
line s = i : (line $ rest)
  where i = read x :: Int
        x = take 1 s
        rest = drop 1 s

-- Read each line as a list of integers
readLines :: IO ([[Int]])
readLines = do
  result <- try getLine :: IO (Either SomeException String)
  case result of
    Left _ -> return []
    Right l -> do
      rest <- readLines
      return (line l : rest)

binary :: [Int] -> Int
binary [] = 0
binary l = (shiftL (head l) (length l - 1)) .|. (binary $ tail l)

common lines = commonz lines zero
  where zero n len = n < (div len 2)

commonz lines zero = map switch sums
  where switch n
          | zero n len = 0
          | otherwise = 1
        sums = foldl (\ l r -> zipWith (+) l r) (head lines) (tail lines)
        len = length lines

one lines = (gamma,  eps)
  where eps = (complement gamma) .&. mask
        gamma = binary rounded
        mask = complement ((complement 0) `shiftL` bits)
        bits = length rounded
        rounded = common lines

strip :: [[Int]] -> Int -> Int -> [[Int]]

strip (l:ls) match bit
  | (l !! bit) == match = l : (strip ls match bit)
  | otherwise = strip ls match bit

strip [] _ _ = []


two :: [[Int]] -> Int -> ([[Int]] -> [Int]) -> Int
two [l] _ _ = binary l
two lines bit patt = two (strip lines match bit) (bit + 1) patt
  where match = (patt lines) !! bit

oxy lines = two lines 0 common
co2 lines = two lines 0 least
  where least ls = commonz ls zero
        zero n len
          | mod len 2 == 1 = n > (div len 2)
          | otherwise = n >= (div len 2)


main = do
  -- Read all lines
  lines <- readLines

  -- Part one
  let (gamma, eps) = one lines
  print (gamma * eps)

  -- Part two
  print ((oxy lines) * (co2 lines))
