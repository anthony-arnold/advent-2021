module Main where
import qualified Data.Set as Set
import Control.Exception

readDots = do
  line <- getLine
  case line of
    "" -> return []
    otherwise -> do
      let xs = takeWhile (/=',') line
      let ys = drop (1 + length xs) line
      let x = read xs :: Int
      let y = read ys :: Int
      rest <- readDots
      return ((x,y):rest)

readFolds = do
  r <- try getLine :: IO (Either SomeException String)
  case r of
    Left _ -> return []
    Right line -> do
      others <- readFolds
      let e = takeWhile (/='=') line
      let d = head $ drop ((length e) - 1) line
      let ns = drop (1 + length e) line
      let n = read ns :: Int
      return ((d,n):others)

enc w (x, y) = y*w+x
dec w e = (mod e w, div e w)

folded coord n e w
  | coord == 'x' && x > n = (n - (x - n), y)
  | coord == 'y' && y > n = (x, n - (y - n))
  | otherwise = (-1, -1)
  where (x, y) = dec w e

foldOne (coord, n) g w e
  | x >= 0 && y >= 0 = Set.delete e $ Set.insert (enc w (x, y)) g
  | otherwise = g
  where (x, y) = folded coord n e w

fold f g w = foldl (\ng e -> foldOne f ng w e) g g

sym w x y g
  | Set.member e g = '*'
  | otherwise = ' '
  where e = enc w (x, y)

outputLn w maxx maxy  y g
  | y > maxy = return ()
  | otherwise = do
      print [sym w x y g | x <- [0..maxx]]
      outputLn w maxx maxy (1 + y) g

output w g = do
  outputLn w maxx maxy 0 g
    where maxx = maximum $ map fst d
          maxy = maximum $ map snd d
          d = map (dec w) $ Set.toList g

main = do
  dots <- readDots
  folds <- readFolds
  let w = maximum $ map fst dots
  let g = Set.fromList $ map (enc w) dots
  print $ length (fold (head folds) g w)
  output w $ foldl (\ng f -> fold f ng w) g folds
