module Main where
import Control.Exception
import qualified Data.Map as Map

data Point = Pt Int Int deriving Eq
data Line = Ln Point Point

instance Ord Point where
  (<=) (Pt a b) (Pt a' b')
    | a == a' && b <= b' = True
    | a < a' = True
    | otherwise = False

x (Pt a _) = a
y (Pt _ b) = b

start (Ln a _) = a
end (Ln _ b) = b

diff (Ln a b) = Pt (x a - x b) (y a - y b)
hori (Ln a b) = (y a) == (y b)
vert (Ln a b) = (x a) == (x b)
diag l = (abs $ x d) == (abs $ y d)
  where d = diff l

readI :: String -> Int
readI s = read s :: Int

toInstruction ::  String -> Line
toInstruction s = Ln (Pt (readI first) (readI second)) (Pt (readI third) (readI fourth))
  where first = takeWhile (/=',') s
        second = takeWhile (/=' ') (drop (length first + 1) s)
        third = takeWhile (/=',') (drop (length first + length second + 5) s)
        fourth = drop (length first + length second + length third + 6) s

getInstructions ::  IO ([Line])
getInstructions = do
  result <- try getLine :: IO (Either SomeException String)
  case result of
    Left _ -> return []
    Right line -> do
      case line of
        "" -> return []
        otherwise -> do
          rest <- getInstructions
          return (toInstruction line : rest)

toward :: Line -> Line
toward (Ln (Pt a b) (Pt p q)) = Ln (Pt v w) (Pt p q)
  where v = a + a'
        w = b + b'
        a'
          | a < p = 1
          | a > p = -1
          | otherwise = 0
        b'
          | b < q = 1
          | b > q = -1
          | otherwise = 0

walk :: Map.Map Point Int -> [Line] -> Map.Map Point Int
walk m [] = m
walk m ((Ln a b):lines)
  | a == b = walk updated lines
  | otherwise = walk updated (next:lines)
  where updated = Map.insert a (v + 1) m
        v = Map.findWithDefault 0 a m
        next = toward (Ln a b)

ans :: [Line] -> Int
ans lines = summarise walked
  where
    walked = walk begin lines
    begin = Map.empty :: Map.Map Point Int
    summarise w = Map.foldr cntr 0 w
    cntr v tot = if v > 1 then tot+1 else tot

main = do
  instr <- getInstructions
  print (ans $ filter (\i -> hori i || vert i) instr)
  print (ans $ filter (\i -> hori i || vert i || diag i) instr)
