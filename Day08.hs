module Main where
import Control.Exception
import Data.List
import qualified Data.Map as Map

numUnique n
  | n >= 10 = (numUnique $ mod n 10) + (numUnique $ div n 10)
  | elem n [1, 4, 7, 8] = 1
  | otherwise = 0

number m (d:ds) n
  | null ds = p
  | otherwise = number m ds p
  where p = n * 10 + (Map.findWithDefault 0 d m)

unique m [] = m
unique m (clue:clues)
  | n == 2 = unique (Map.insert clue 1 m) clues
  | n == 3 = unique (Map.insert clue 7 m) clues
  | n == 4 = unique (Map.insert clue 4 m) clues
  | n == 7 = unique (Map.insert clue 8 m) clues
  | otherwise = unique m clues
  where n = length clue

allpairs [] _ = []
allpairs _ [] = []
allpairs (l1:l1s) (l2:l2s) = (l1, l2) : rest
  where rest = (allpairs [l1] l2s) ++ (allpairs l1s (l2:l2s))

subset sub sup = all (\l -> elem l sup) sub

zero m clues = Map.insert czero 0 m
  where czero = head $ filter(\c -> Map.notMember c m) nsix
        nsix = filter(\c -> length c == 6) clues

two m clues = Map.insert ctwo 2 m
  where ctwo = head $ filter(\c -> Map.notMember c m) nfive
        nfive = filter(\c -> length c == 5) clues

fivesix m clues = Map.insert csix 6 (Map.insert cfive 5 m)
  where (cfive, csix) = head $ filter(\(f, s) -> subset f s) nfivesix
        nfivesix = allpairs nfive nsix
        nfive = filter(\c -> length c == 5) rest
        nsix = filter(\c -> length c == 6) rest
        rest = filter(\c -> Map.notMember c m) clues

three m clues = Map.insert cthree 3 m
  where cthree = head $ filter(\c -> subset cone c) nfive
        cone = head $ filter(\c -> length c == 2) clues
        nfive = filter(\c -> length c == 5) clues

nine m clues = Map.insert cnine 9 m
  where cnine = head $ filter (\c -> subset cfour c) nsix
        cfour = head $ filter (\c -> length c == 4) clues
        nsix = filter (\c -> length c == 6) clues

deduce clues = m0
  -- Read these bottom-up
  where m0 = zero m2 clues -- 0 is the only length-6 remaining
        m2 = two m56 clues -- 2 is the only length-5 remaining
        m56 = fivesix m3 clues -- 5 is a subset of 6
        m3 = three m9 clues -- 3 is the only length-5 superset of 1
        m9 = nine mu clues -- 9 is the only length-6 superset of 4
        mu = unique m clues -- find all the unique ones
        m = Map.empty :: Map.Map String Int

output s = number deduct trailer 0
  where deduct = deduce header
        trailer = drop (1 + length header) tok
        header = takeWhile (/="|") tok
        tok = map sort (words s)

input = do
  result <- try getLine :: IO (Either SomeException String)
  case result of
    Left _ -> return []
    Right line -> do
      rest <- input
      return (line : rest)

main = do
  x <- input
  let o = map output x
  let u = map numUnique o
  print (sum u)
  print (sum o)
