module Main where
import Control.Exception
import qualified Data.Map as Map

readLines = do
    l <- try getLine :: IO (Either SomeException String)
    case l of
        Left _ -> return (Map.empty :: Map.Map String Char)
        Right line -> do
            lines <- readLines
            let left = takeWhile (/='-') line
            let right = drop (3 + length left) line
            let left' = take ((length left) - 1) left
            let right' = head right
            return (Map.insert left' right' lines)

pairs s m
    | length s < 2 = m
    | otherwise = pairs (tail s) (Map.insert p n m)
    where p = take 2 s
          n = (1 + Map.findWithDefault 0 p m)

spairs s = pairs s (Map.empty :: Map.Map String Int)

applyL [] t _ _ = t
applyL (p:ps) t m t0
    | c /= '.' = x
    | otherwise = applyL ps t m t0
    where c = Map.findWithDefault '.' p m
          f = [head p, c]
          s = [c, last p]
          y = Map.union (Map.fromList [(f,nf), (s,ns)]) u
          np = Map.findWithDefault 0 p t0
          np' = (Map.findWithDefault 0 p t) - np
          u = Map.insert p np' t
          nf = np + Map.findWithDefault 0 f u
          ns = np + Map.findWithDefault 0 s u
          x = applyL ps y m t0

apply s m = applyL (Map.keys s) s m s
    
applyN s _ 0 = s
applyN s m t = applyN x m (t - 1)
    where x = apply s m

count s a = foldl (\m (c:_, n) -> Map.insert c (n+Map.findWithDefault 0 c m) m) e v
    where e = Map.fromList [(a, 1)]
          v = Map.toList s

by f l r 
    | f (snd l) (snd r) = l
    | otherwise = r

minmax s a = (snd mx, snd mn)
    where mx = foldl (by (>)) (' ', 0) pairs
          mn = foldl (by (<)) mx pairs
          pairs = Map.toList c 
          c = count s a

main = do
   h <- getLine
   _ <- getLine
   lines <- readLines
   let s = applyN (spairs h) lines 10
   
   let (mx, mn) = minmax s (last h)
   print $ mx - mn

   let s' = applyN s lines 30
   let (mx', mn') = minmax s' (last h)
   print $ mx' - mn'
