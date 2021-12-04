module Main where
import Control.Exception
import Data.Array.IO

-- Split a string
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) trimmed
          trimmed = dropWhile (==c) s
          rest = drop (length firstWord + 1) trimmed

toInts :: Char ->  String -> [Int]
toInts c s = map (\t -> read t :: Int) tokens
  where tokens = split c s

getNumbers :: IO ([Int])
getNumbers = do
  line <- getLine
  return (toInts ',' line)

getBoard :: IO ([[Int]])
getBoard = do
  result <- try getLine :: IO (Either SomeException String)
  case result of
    Left _ -> return []
    Right line -> do
      case line of
        "" -> return []
        otherwise -> do
          rest <- getBoard
          return (toInts ' ' line : rest)


getBoards :: IO ([[[Int]]])
getBoards = do
  board <- getBoard
  case board of
    [] -> return []
    otherwise -> do
      rest <- getBoards
      return (board : rest)

hasWonH board = any (all (==0)) board
hasWonV board = any (\i -> all (==0) (map (\r -> r !! i) board)) [0, 1, 2, 3, 4]
hasWon board = hasWonH board || hasWonV board

score board = foldl (+) 0 (concat board)

markCol :: Int -> Int -> Int
markCol n v
  | n == v = 0
  | otherwise = v

markRow :: Int -> [Int] -> [Int]
markRow n row = map (markCol n) row

markBoard :: Int -> [[Int]] -> [[Int]]
markBoard n rows = map (markRow n) rows

one :: [Int] -> [[[Int]]] -> Int
one n boards = case winners of
                 [] -> one (tail n) marked
                 otherwise -> (score (head winners)) * (head n)
  where
    winners = filter hasWon marked
    marked = map (markBoard (head n)) boards


two :: [Int] -> [[[Int]]] -> Int
two n boards = case losers of
                 [] -> (score (head marked)) * (head n)
                 otherwise -> two (tail n) losers
  where
    losers = filter (\b -> not (hasWon b)) marked
    marked = map (markBoard (head n)) boards

main = do
  n <- getNumbers

  -- Discard empty line
  getLine

  -- Read all boards
  boards <- getBoards

  -- Part 1
  print (one n boards)

  -- Part 2
  print (two n boards)
