module Main where

-- Split a string
split :: Char -> String -> [String]
split _ "" = []
split c s = firstWord : (split c rest)
    where firstWord = takeWhile (/=c) s
          rest = drop (length firstWord + 1) s

-- Read each line as a string (direction) followed by an integer (magnitude)
readInstructions :: IO ([(String, Int)]) 
readInstructions = do
    l <- getLine
    case l of
        "" -> return []
        otherwise -> do
            instrs <- readInstructions
            return (i : instrs)
            where
                i = (d, n)
                d = head x
                n = read (x !! 1) :: Int
                x = split ' ' l
                
-- Part one instructions
move :: (String, Int) -> (Int, Int) -> (Int, Int)
move (d, n) (h, v) 
    | d == "forward" = (h + n, v)
    | d == "up" = (h, v - n)
    | d == "down" = (h, v + n)

-- Part two instructions.
aim :: (String, Int) -> (Int, Int, Int) -> (Int, Int, Int)
aim (d, n) (h, v, a)
    | d == "forward" = (h + n, v + a * n, a)
    | d == "up" = (h, v, a - n)
    | d == "down" = (h, v, a + n)

-- Process each instruction in part one
one :: [(String, Int)] -> (Int, Int) -> (Int, Int)
one [] p = p
one instr p = one (tail instr) (move (head instr) p)

-- Process each instruction in part two
two :: [(String, Int)] -> (Int, Int, Int) -> (Int, Int, Int)
two [] p = p
two instr p = two (tail instr) (aim (head instr) p)

main = do
    -- Read all instructions into a list of 2-tuples (string, int)
    i <- readInstructions

    -- Answer part one
    let (x, y) = one i (0, 0)
    print (x * y)

    -- Answer part two
    let (u, v, w) = two i (0, 0, 0)
    print (v * u)
    
