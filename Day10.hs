module Main where
import Control.Exception
import Data.List

score '(' = 1
score '[' = 2
score '{' = 3
score '<' = 4

score' s = foldl(\t c -> t*5 + score c) 0 s

parse :: String -> String -> (Int, Int)
parse s "" = (0, score' s)
parse s (c:line) 
    | elem c ['<', '{', '(', '['] = parse (c:s) line
    | c == '>' && (head s) /= '<' = (25137, 0)
    | c == ')' && (head s) /= '(' = (3, 0)
    | c == ']' && (head s) /= '[' = (57, 0)
    | c == '}' && (head s) /= '{' = (1197, 0)
    | otherwise = parse (tail s) line

readLines = do
    result <- try getLine :: IO (Either SomeException String)
    case result of
        Left _ -> return []
        Right line -> do
            rest <- readLines
            return ((parse [] line) : rest)

main = do
    scores <- readLines
    print (sum $ map fst scores)
    let scores2 = sort $ filter (/= 0) $ map snd scores
    let n = div (length scores2) 2
    print (scores2 !! n)
