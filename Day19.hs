module Main where
import Control.Exception
import Data.List
import Data.Maybe
import Data.Bool
import qualified Data.Set as Set
import qualified Data.Map as Map


data Coord = X | Y | Z deriving Show
data Orientation = Ori (Coord, Bool) (Coord, Bool) (Coord, Bool) deriving Show

rotations [l, u, r] [x, y, z]
  | foldl (&&) l [u, r] = [o]
  | otherwise = o:rest
  where o = Ori (x, l) (y, u) (z, r)
        rest = rotations r' [x, y, z]
        r'
          | u && r = [True, False, False]
          | r = [l, True, False]
          | otherwise = [l, u, True]

orientations = concat $ map (rotations [False | _ <- [0..2]]) $ permutations [X, Y, Z]

pick (x, _, _) X = x
pick (_, y, _) Y = y
pick (_, _, z) Z = z

positive p = bool (-1) 1 p

rotate' (Ori (cx, _) (cy, _) (cz, _)) coord = (pick coord cx, pick coord cy, pick coord cz)
frotate (Ori (_, no) (_, nu) (_, nr)) = (fno, fnu, fnr)
  where fno = positive no
        fnu = positive nu
        fnr = positive nr

rotate orientation coord = (x * fno, y * fnu, z * fnr)
  where (x, y, z) = rotate' orientation coord
        (fno, fnu, fnr) = frotate orientation

translate (x', y', z') (x, y, z) = (x+x', y+y', z+z')

transform r d c = translate d $ rotate r c

beacon s = (x, y, z)
  where x = (read xs :: Int)
        y = (read ys :: Int)
        z = (read zs :: Int)
        xs = takeWhile (/=',') s
        ys = takeWhile (/=',') (drop (1 + length xs)  s)
        zs = drop (2 + (length xs) + (length ys)) s

beacons = do
  r <- try getLine :: IO (Either SomeException String)
  case r of
    Left _ -> return Set.empty
    Right line -> do
      case line of
        "" -> return Set.empty
        otherwise -> do
          rest <- beacons
          return $ Set.insert (beacon line) rest

scanners = do
  r <- try getLine :: IO (Either SomeException String)
  case r of
    Left _ -> return []
    Right _ -> do
       s <- beacons
       rest <- scanners
       return $ s:rest

diff (x, y, z) (u, v, w) = (x - u, y - v, z - w)

overlap src dst srcTo dstTo = f
  where x = Set.intersection src (Set.map (translate df) dst)
        df = diff srcTo dstTo
        f
          | length x >= 12 = Just $ df
          | otherwise = Nothing

findOverlap'' src dst srcTo
  | Set.size dst < 12 = Nothing
  | otherwise = f
  where f = case g of
              Nothing -> findOverlap'' src (Set.delete d dst) srcTo
              otherwise -> g
        g = overlap src dst srcTo d
        d = Set.elemAt 0 dst


findOverlap' src dst
  | Set.size src < 12 = Nothing
  | otherwise = f
  where f = case g of
              Nothing -> findOverlap' (Set.delete s src) dst
              otherwise -> g
        g = findOverlap'' src dst s
        s = Set.elemAt 0 src


findOverlap src dst [] = Nothing
findOverlap src dst (r:rs) = case rel of
    Nothing -> findOverlap src dst rs
    Just d -> Just (r, d)
  where rel = findOverlap' src rotated
        rotated = Set.map (rotate r) dst


reconcile' origin ss = case ovl of
                 Nothing -> reconcile' origin (Set.delete s ss)
                 Just x -> (s, x)
  where ovl = findOverlap origin s orientations
        s = Set.elemAt 0 ss

reconcile origin s
  | Set.null s = (origin, [(0, 0, 0)])
  | otherwise = (full, d:x)
  where (full, x) = reconcile origin' s'
        origin' = Set.union origin xfrm
        s' = Set.delete c s
        xfrm = Set.map (transform r d) c
        (c, (r, d)) = reconcile' origin s

mag a b = sum $ map abs $ [x, y, z]
  where (x, y, z) = diff a b

dist' xs y = maximum $ map (mag y) xs
dist xs ys = maximum $ map (dist' xs) ys

main = do
    s <- scanners
    let (origin, d) = reconcile (head s) (Set.fromList $ tail s)
    print $ length $ origin
    print $ dist d d
