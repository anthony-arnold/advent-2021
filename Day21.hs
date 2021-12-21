module Main where
import Data.List
import Data.Bool

position s = read n :: Int
  where n = last $ words s

deterministicDie = cycle [1..100]
three (a:(b:(c:rest))) = [a,b,c] : (three rest)

move start = scanl once (start, (0, 0), 0) $ three deterministicDie
  where once ((p1, p2), (s1, s2), rolls) n = ((p2, np1), (s2, ns1), 3 + rolls)
          where np1 = bool m 10 (m == 0)
                m = mod (p1 + sp) 10
                ns1 = s1 + np1
                sp = sum n

finished (_, (_, s), _) = s >= 1000

main = do
  l1 <- getLine
  l2 <- getLine

  let start = (position l1, position l2)
  let moves = dropWhile (not . finished) $ move start
  let ((_, _), (loser, _), rolls) = head moves
  print $ loser * rolls
