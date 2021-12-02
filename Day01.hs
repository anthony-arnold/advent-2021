module Main where

-- Read stdin line by line and convert to a list of integers
toList :: IO ([Int])
toList = do
    l <- getLine
    case l of 
        "" -> return []
        otherwise -> do
            ms <- toList
            return (m : ms)
            where
                m = read l :: Int

-- Compute the total number of window sum increases over a list.
-- w: window size
-- l: list to inspect
-- p: previous total 
increases :: Int -> [Int] -> Int -> Int
increases w l p
    | length l < (w + 1) = p
    | (l !! w) > (head l) = increases w (tail l) p+1
    | otherwise = increases w (tail l) p
    
-- Short-hand to get the answer (discards temporaries).
answer :: Int -> [Int] -> Int
answer w l = increases w l 0

main = do
    l <- toList
    let i = answer 1 l 
    let i3 = answer 3 l
    print i
    print i3
    
