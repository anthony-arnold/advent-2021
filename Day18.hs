module Main where
import Data.Char
import Control.Exception
import Data.List

readNum (c:str) d = this
  where this
          | isDigit c = ([(digitToInt c, d)], str)
          | c == '[' = (left ++ right, rem)
        (left, lrem) = readNum str (1+d)
        (right, rrem) = readNum (drop 1 lrem) (1+d)
        rem
          | head rrem == ',' = rrem
          | otherwise = tail rrem

numbers = do
  r <- try getLine :: IO (Either SomeException String)
  case r of
    Left _ -> return []
    Right line -> do
      rest <- numbers
      return ((fst $ readNum line 0):rest)

split [] = ([], False)
split ((i,d):ns)
  | i >= 10 = ([(q, d+1), (q+r, d+1)] ++ ns, True)
  | otherwise = ((i,d):rest, chg)
  where (q, r) = divMod i 2
        (rest, chg) = split ns

explode [] = ([], False)
explode ((l,d):[(p1,5), (p2,5)]) = ([(l+p1, d), (0, 4)], True)
explode ((p1, 5):((p2, 5):((r, d):ns))) = ((0, 4):rest, True)
  where rest = (r+p2, d):ns
explode ((l, d1):((p1, 5):((p2, 5):((r, d2):ns)))) = ([(l+p1, d1), (0, 4)] ++ rest, True)
  where rest = (r+p2, d2):ns
explode (n:ns) = (n:rest, chg)
  where (rest, chg) = explode ns

reduce n
  | chg = reduce reduced
  | otherwise = n
  where (reduced, chg)
          | echg = (exploded, True)
          | otherwise = split n
        (exploded, echg) = explode n

add l r = reduce $ map inc $ l++r
  where inc (n, d) = (n, d+1)

mag' [] _ = []
mag' [n] _ = [n]
mag' ((l, d1):((r, d2):ns)) d
  | d1 == d && d2 == d = (3*l + 2*r, d-1):(mag' ns d)
  | otherwise = (l, d1):(mag' ((r, d2): ns) d)

mag [(n, _)] = n
mag n = mag $ mag' n d
  where d = maximum $ map snd $ n

maxmag [_] = 0
maxmag (n:ns) = maximum $ (maxmag ns):mags
    where mags = ord ++ rev
          ord = map mag $ map (\(n, m) -> add n m) comb
          rev = map mag $ map (\(n, m) -> add m n) comb
          comb = map (\m -> (n, m)) ns

main = do
  n <- numbers
  print $ mag $ foldl add (head n) (tail n)
  print $ maxmag n
