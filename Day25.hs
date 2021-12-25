module Main where
import Control.Exception
import Data.Array
import Data.List

getGrid y = do
  r <- try getLine :: IO (Either SomeException String)
  case r of
    Left _ -> return []
    Right s -> do
      rest <- getGrid (y + 1)
      let row = map (\(x, c) -> ((x, y), c)) $ zip [0..] s
      return $ row:rest

right (x, y) (mx, my)
  | x' > mx = (0, y)
  | otherwise = (x', y)
  where x' = x + 1

down (x, y) (mx, my)
  | y' > my = (x, 0)
  | otherwise = (x, y')
  where y' = y + 1

move coord grid b = grid // [(dst, c), (coord, '.')]
  where c = grid!coord
        dst = case c of
                '>' -> right coord b
                'v' -> down coord b

members grid = partition (\c -> '>' == snd c) cs
  where cs = filter (\(_, c) -> c `elem` ">v") as
        as = assocs grid

movers grid cs b = map fst $ filter canMove cs
  where canMove (c, t) = grid!dst == '.'
          where dst = case t of
                  '>' -> right c b
                  'v' -> down c b

step grid b = next
  where next = foldl (\g c -> move c g b) shifted $ movers shifted downs b
        shifted = foldl (\g c -> move c g b) grid $ movers grid rights b
        (rights, downs) = members grid

steps grid b n
  | next == grid = n'
  | otherwise = steps next b n'
  where next = step grid b
        n' = n + 1

main = do
  l <- getGrid 0
  let b = (length (head l) - 1, length l - 1)
  let g = array ((0, 0), b) $ concat l
  print $ steps g b 0
