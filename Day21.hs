module Main where
import Data.List
import Data.Bool
import Data.Tuple

deterministic = cycle [1..100]
three (a:(b:(c:rest))) = [a,b,c] : (three rest)

move start = scanl once (start, (0, 0), 0) $ three deterministic
  where once ((p1, p2), (s1, s2), rolls) n = ((p2, np1), (s2, ns1), 3 + rolls)
          where (np1, ns1) = score p1 s1 n

finished (_, (_, s), _) = s >= 1000

data Player = One | Two deriving Eq
pass One = Two
pass Two = One

dirac = [3..9]
freq 3 = 1
freq 4 = 3
freq 5 = 6
freq 6 = 7
freq 7 = 6
freq 8 = 3
freq 9 = 1

move' (p1, p2) (s1, s2) turn trace r
 | ns1 >= 21 && turn == trace = f
 | ns1 >= 21 = 0
 | otherwise = f * (sum $ map (move' np ns (pass turn) trace) dirac)
  where (np1, ns1) = score p1 s1 [r]
        np = (p2, np1)
        ns = (s2, ns1)
        f = freq r

part2 start trace = sum $ map (move' start (0, 0) One trace) dirac

score p s r = (np, ns)
  where np = bool m 10 (m == 0)
        m = mod (p + sp) 10
        ns = s + np
        sp = sum r

position s = read n :: Int
  where n = last $ words s

main = do
  l1 <- getLine
  l2 <- getLine

  let start = (position l1, position l2)
  let moves = dropWhile (not . finished) $ move start
  let ((_, _), (loser, _), rolls) = head moves
  print $ loser * rolls
  print $ max (part2 start One) (part2 start Two)
