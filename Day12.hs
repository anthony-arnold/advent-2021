module Main where
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Char
import Control.Exception

lower l = l /= "end" && l /= "start" && isLower (head l)

addEdge b e m = Map.insert e (b:bs) (Map.insert b (e:es) m)
  where es = Map.findWithDefault [] b m
        bs = Map.findWithDefault [] e m

readEdges = do
  r <- try getLine :: IO (Either SomeException String)
  case r of
    Left _ -> return (Map.empty :: Map.Map String [String])
    Right line -> do
      others <- readEdges
      let begin = takeWhile (/='-') line
      let end = drop (1 + length begin) line
      return (addEdge begin end others)

numPaths d v g q
  | q == "end" = 1
  | isUpper (head q) = sum $ map (numPaths d v g) adj
  | q == d = sum $ map (numPaths "" v g) adj
  | otherwise = sum $ map (numPaths d v' g) adj
  where v' = Set.insert q v
        adj = filter (\e -> Set.notMember e v) (Map.findWithDefault [] q g)

paths g = numPaths "" e g "start"
    where e = Set.empty :: Set.Set String

paths' g = d - s
    where v = Set.empty :: Set.Set String
          d = sum $ map (\l -> numPaths l v g "start") ls
          s = (paths g) * ((length ls) - 1)
          ls = filter lower (Map.keys g)

main = do
    g <- readEdges
    print $ paths g
    print $ paths' g
