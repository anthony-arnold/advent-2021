module Main where
import Data.Char
import Data.Bits
import Data.Bool

data Packet =
    Literal Int Int |
    Operator Int Int [Packet]

bits h 
    | h <= '9' = ord h - ord '0'
    | otherwise = 10 + ord h - ord 'A'
bin 0 = "0"
bin 1 = "1"
bin h = (bin $ div h 2) ++ (bin $ mod h 2)

binStr "" = ""
binStr (h:hex) = str ++ binStr hex
    where str = (take (4 - length hstr) $ repeat '0') ++ hstr
          hstr = bin $ bits h

dec :: String -> Int
dec "1" = 1
dec "0" = 0
dec (b:bits) = (shiftL (dec [b]) (length bits)) + dec bits

literalS ('1':bits) = ((take 4 bits) ++ rest, 5 + len)
    where (rest, len) = literalS $ drop 4 bits
literalS ('0':bits) = (take 4 bits, 5)
literal bits = (dec str, len)
    where (str, len) = literalS bits

npackets 0 bits = ([], bits)
npackets n bits = (p:rest, rem')
    where (p, rem) = packets bits
          (rest, rem') = npackets (n-1) rem

lpackets 0 _ = []
lpackets l bits = p:rest
    where (p, rem) = packets bits
          rest = lpackets (l - (length bits - length rem)) rem

subpackets ('0':bits) = (subs, rem)
    where subs = lpackets len $ drop 15 bits
          len = dec $ take 15 bits
          rem = drop (len + 15) bits

subpackets ('1':bits) = (subs, rem)
    where (subs, rem) = npackets num $ drop 11 bits
          num = dec $ take 11 bits

packets bits = (p, rem)
    where p
            | ptype == 4 = Literal version lit
            | otherwise = Operator version ptype subs
          rem
            | ptype == 4 = drop (6 + llen) bits
            | otherwise = remsubs
          version = dec $ take 3 bits
          (lit, llen) = literal $ drop 6 bits
          (subs, remsubs) = subpackets $ drop 6 bits
          ptype = dec $ take 3 $ drop 3 bits

vsum (Literal v _) = v
vsum (Operator v _ p) = v + (sum $ map vsum p)

gt [l, r] = bool 0 1 (l > r)
lt [l, r] = bool 0 1 (l < r)
eq [l, r] = bool 0 1 (l == r)

op :: Int -> ([Int] -> Int)
op 0 = sum
op 1 = product
op 2 = minimum
op 3 = maximum
op 5 = gt
op 6 = lt
op 7 = eq

value (Literal _ v) = v
value (Operator _ ptype p) = op ptype $ map value p

main = do
    hex <- getLine
    let (p, _) = packets $ binStr hex
    print $ vsum p
    print $ value p
