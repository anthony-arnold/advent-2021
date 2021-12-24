module Main where
import Data.List
import Data.Char
import Data.Bool
import SPFA

data State = St [Char] [[Char]] deriving Show

instance Ord State where
  (<=) (St h1 r1) (St h2 r2) = (h1, r1) <= (h2, r2)

instance Eq State where
  (==) (St h1 r1) (St  h2 r2) = (h1, r1) == (h2, r2)

finished (St hall r) = all (=='.') hall && all full rooms
  where rooms = zip r ['A'..'Z']
        full (pods, c) = all (==c) pods

target c = (1 + ord c - ord 'A') * 2

cost 'A' = 1
cost 'B' = 10
cost 'C' = 100
cost 'D' = 1000

distance c d path room = (cost c) * (d + length path - length room)

addRoom rooms c = preRooms ++ newRoom : postRooms
  where (preRooms,room:postRooms) = splitAt (ord c - ord 'A') rooms
        newRoom = c:room

remRoom rooms i = preRooms ++ newRoom : postRooms
  where (preRooms,room:postRooms) = splitAt i rooms
        newRoom = tail room

setHall hall i c = preHall ++ c : postHall
  where (preHall,_:postHall) = splitAt i hall

path hall i t = drop start $ take (1+end) hall
  where (start, end) = (min i t, max i t)

nextHall d w (St hall rooms) i
  | i >= length hall = []
  | c == '.' = next
  | notBlocked && homeValid = home:next
  | otherwise = next
  where next = nextHall d w (St hall rooms) (1+i)
        span = path hall (bool (i-1) (i+1) (i < t)) t
        t = target c
        c = hall!!i
        notBlocked = all (=='.') span
        room = rooms!!(ord c - ord 'A')
        homeValid = all (==c) room
        newHall = setHall hall i '.'
        newRooms = addRoom rooms c
        home = ((w + distance c d span room), St newHall newRooms)

allExits d w (St hall rooms) i j
  | j >= length hall = []
  | any (==j) $ map (*2) [1..length rooms] = next
  | blocked = next
  | otherwise = newState:next
  where next = allExits d w (St hall rooms) i (1+j)
        blocked = any (/='.') span
        span = path hall (2 * (i+1)) j
        room = rooms!!i
        c = head room
        newRooms = remRoom rooms i
        newHall = setHall hall j c
        newState = ((w + distance c d span room), St newHall newRooms)

nextExits d w (St hall rooms) i
  | i >= length rooms = []
  | length room == 0 = next
  | all (==c) room = next
  | otherwise = concat [allExits d w (St hall rooms) i 0, next]
  where room = rooms!!i
        c = chr (i + ord 'A')
        next = nextExits d w (St hall rooms) (1+i)

nextStates d w st = concat [nextHall d w st 0, nextExits d w st 0]

stackDown [] _ = []
stackDown (r:rs) (a:as) = (r ++ [a]):(stackDown rs as)
amph rooms s = stackDown rooms as
    where as = filter isAlpha s

cheapest d state = case shortest next finished (0, state) of
                   Nothing -> error "oops"
                   Just (w, v) -> w
  where next (w, v) = nextStates d w v

main = do
    -- Drop first line
    getLine

    -- Read hallway
    h <- getLine
    let hall = filter (=='.') h

    -- Get the amphipods
    s1 <- getLine
    s2 <- getLine

    let rooms = amph (amph [[], [], [], []] s1) s2
    let depth = length $ head rooms
    print $ cheapest depth (St hall rooms)

    let rooms' = foldl (\a s -> amph a s) [[],[],[],[]] [s1,"DCBA","DBAC",s2]
    let depth' = length $ head rooms'
    print $ cheapest depth' (St hall rooms')
