module Main where
import Control.Exception
import qualified Data.Map as Map
import Data.Array
import Data.Bool
import Data.List

getImg' y = do
  r <- try getLine :: IO (Either SomeException String)
  case r of
    Left _ -> return Map.empty
    Right l -> do
      rest <- getImg' (1+y)
      return $ Map.union rest points
        where points = Map.fromList desc
              desc = [((x, y), l!!x == '#') | x <- [0..length l - 1]]

getImg = do
  img <- getImg' 0
  return img

bin x y img dflt = foldl dec 0 region
  where region = [(x+x', y+y') | y' <- [-1..1], x' <- [-1..1]]
        dec n c = n*2 + (bool 0 1 $ Map.findWithDefault dflt c img)

out x y img algo dflt = algo!v
  where v = bin x y img dflt

ranges' r [] = r
ranges' (minX, maxX, minY, maxY) ((x, y):cs) = ranges' upd cs
  where upd = (min minX x, max maxX x, min minY y, max maxY y)
ranges cs = ranges' (0, 0, 0, 0) cs

expand (minX, maxX, minY, maxY) = (minX-1, maxX+1, minY-1, maxY+1)

apply algo img dflt (minX, maxX, minY, maxY) = next
  where next = foldl enhance img outputs
        enhance m (x, y, on) = Map.insert (x, y) on m
        outputs = [(x, y, out x y img algo dflt) | x <- [minX..maxX], y <- [minY..maxY]]

enhancements _ img 0 _ _ = img
enhancements algo img times dflts bnds = enhanced
  where enhanced = enhancements algo next (times - 1) ((tail dflts) ++ [dflt]) ex
        next = apply algo img dflt ex
        dflt = head dflts
        ex = expand bnds

writeLn y img xbound = do
  let s = [bool '.' '#' $ Map.findWithDefault False (x, y) img | x <- xbound]
  print s

writeLns _ [] _ = return ()
writeLns img (y:ys) xbound  = do
  writeLn y img xbound
  writeLns img ys xbound

write img bounds = do
  writeLns img bounds bounds

answer algo img times = sum $ map (bool 0 1) $ Map.elems $ enhanced
  where zero = algo!0
        enhanced = enhancements algo img times [False, zero] (ranges $ Map.keys img)

main = do
  a <- getLine
  let algo = array (0, length a - 1) [(i, a!!i == '#') | i <- [0 .. length a - 1]]
  let zero = algo!0

  -- Discard empty line
  getLine

  img <- getImg

  print $ answer algo img 2
  print $ answer algo img 50
