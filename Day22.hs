module Main where
import Control.Exception
import Data.List
import Data.Maybe
import Data.Bool


data Cuboid = Cb (Int, Int) (Int, Int) (Int, Int)
data Instruction = Instr Bool Cuboid


split _ "" = []
split c s = i:rest
    where i = takeWhile (/=c) s
          rest = split c $ drop (1 + length i) s


toRange s = ((read lo :: Int), (read hi :: Int))
    where lo = takeWhile (/='.') s
          hi = drop (2 + length lo) s


toCube s = Cb (toRange x) (toRange y) (toRange z)
    where x = drop 2 (c!!0)
          y = drop 2 (c!!1)
          z = drop 2 (c!!2)
          c = split ',' s


toInstruction s = Instr on cube
    where on = (head w) == "on"
          cube = toCube (w!!1)
          w = words s


getInstructions = do
    r <- try getLine :: IO (Either SomeException String)
    case r of
        Left _ -> return []
        Right l -> do
            rest <- getInstructions
            return ((toInstruction l):rest)


intersection (Cb x y z) (Cb u v w)
    | any (\(a, b) -> a > b) [x', y', z'] = Nothing
    | otherwise = Just $ Cb x' y' z'
    where x' = (max (fst x) (fst u), min (snd x) (snd u))
          y' = (max (fst y) (fst v), min (snd y) (snd v))
          z' = (max (fst z) (fst w), min (snd z) (snd w))


cubes (Cb x y z) = a*b*c
    where a = 1 + (snd x) - (fst x)
          b = 1 + (snd y) - (fst y)
          c = 1 + (snd z) - (fst z)


overlaps _ [] = []
overlaps a ((Instr on b):os) = case (intersection a b) of
                                 Just cube -> (Instr (not on) cube):rest
                                 otherwise -> rest
    where rest = overlaps a os


total [] = 0
total ((Instr on cube):is) = this + rest
    where this
            | on = cubes cube
            | otherwise = -(cubes cube)
          rest = total is


switch [] prev = 0
switch ((Instr on cube):is) prev = t + switch is (prev ++ extra)
    where extra
            | on = (Instr on cube):xsect
            | otherwise = xsect
          xsect = overlaps cube prev
          t = total extra


validRange (a, b) = all (\x -> x >= -50 && x <= 50) [a, b]
valid (Instr _ (Cb x y z)) = all validRange [x, y, z]


main = do
    instructions <- getInstructions
    print $ switch (filter valid instructions) []
    print $ switch instructions []
