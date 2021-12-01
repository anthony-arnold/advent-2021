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

-- Determine the next total by comparing the current window sum
-- with the next sum and increasing the total if needed.
-- x: next window sum
-- y: current total
-- z: current window sum
-- equals: (next total, next window sum)
increased :: Int -> (Int, Int) -> (Int, Int)
increased x (y, z)
    | x > z = (y + 1, x)
    | otherwise = (y, x)

-- Compute the total number of window sum increases over a list.
-- w: window size
-- l: list to inspect
-- p: 2-tuple of previous total and previous window sum
increases :: Int -> [Int] -> (Int, Int) -> (Int, Int)
increases w l p
    | length l < w = p
    | otherwise = increases w r i
        where
            i = increased s p
            s = sum $ take w l
            r = tail l
    
-- Short-hand to get the answer (discards temporaries).
answer w l = r
    where
        (r, _) = increases w l (0, m)
        m = maxBound :: Int

main = do
    l <- toList
    let i = answer 1 l 
    let i3 = answer 3 l
    print i
    print i3
    
