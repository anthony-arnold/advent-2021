module Main where
import Data.List
import Data.Char

exec w z (c1, c2, c3) = z'
  where z'
          | w == x' = dz
          | otherwise = dz * 26 + w + c3
        x' = c2 + mod z 26
        dz = div z c1

test' 0 _ [] = Just []
test' z _ [] = Nothing
test' z is ((26, c2, c3):cs)
  | w < 1 || w > 9 = Nothing
  | otherwise = case r of
                  Just r' -> Just $  w:r'
                  otherwise -> Nothing
  where r = test' z' is cs
        x = c2 + mod z 26
        w
          | x < 0 = 26 + x
          | otherwise = x
        z' = exec w z (26, c2, c3)


test' z (i:is) (c:cs) = case r of
                          Just r' -> Just $ w:r'
                          otherwise -> Nothing
  where r = test' z' is cs
        w = i
        z' = exec w z c

test is = test' 0 is coeff

maxValid i
  | i == 999999 = error "oops"
  | any (==0) is = next
  | otherwise = case t of
                  Just t' -> foldl (\n i -> i + 10*n) 0 t'
                  otherwise -> next
  where t = test is
        is = map (\c -> ord c - ord '0') $ show i
        next = maxValid (i - 1)

minValid i
  | i == 10000000 = error "oops"
  | any (==0) is = next
  | otherwise = case t of
                  Just t' -> foldl (\n i -> i + 10*n) 0 t'
                  otherwise -> next
  where t = test is
        is = map (\c -> ord c - ord '0') $ show i
        next = minValid (i + 1)

coeff = [
  (1, 14, 8),
  (1, 13, 8),
  (1, 13, 3),
  (1, 12, 10),
  (26, -12, 8),
  (1, 12, 8),
  (26, -2, 8),
  (26, -11, 5),
  (1, 13, 9),
  (1, 14, 3),
  (26, 0, 4),
  (26, -12, 9),
  (26, -13, 2),
  (26, -6, 7)]

main = do
  print $ maxValid 9999999
  print $ minValid 1111111
