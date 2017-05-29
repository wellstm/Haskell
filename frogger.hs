import ConsoleIO
import Data.Char
import System.Timeout

data PlayerState = ALIVE | DEAD
type Player = (Pos, PlayerState)

type Length = Int
type Speed = Int
type Truck = (Pos, Length, Speed)

data SlotState = EMPTY | FILLED
type Slot = (Pos, SlotState)
type Slots = (Slot, Slot, Slot)

type QuitState = Bool
type GameState = (Player, [Truck], Slots, QuitState)

data Move = UP | DOWN | LEFT | RIGHT | NOTHING | QUIT

inputTimeout :: Int
inputTimeout = 500000 -- Microseconds

width :: Int
width = 70

height :: Int
height = 20

littleTruck :: Truck
littleTruck = ((0,0), 4, 3)

bigTruck :: Truck
bigTruck = ((0,0), 10, 1)

mediumTruck :: Truck
mediumTruck = ((0,0), 6, 2)

setTruckPos :: Truck -> Pos -> Truck
setTruckPos (oldPos, s, sp) newPos = (newPos, s, sp)

drawBackground :: [IO()]
drawBackground = [writeAt (x,y) "|" | y <- [1..height], x <- [1, width]] ++ 
                 [writeAt (0,0) topBar]
                 where topBar = take width (repeat '-')

drawPlayer :: Player -> [IO()]
drawPlayer (pos, ALIVE) = [writeAt pos "F"]
drawPlayer (pos, DEAD) = [writeAt pos "X"]

drawTruck :: Truck -> IO()
drawTruck (pos, size, _) = writeAt pos (take size (repeat 'T'))

drawTrucks :: [Truck] -> [IO()]
drawTrucks ts = map drawTruck ts

drawSlot :: Slot -> IO()
drawSlot (pos, EMPTY) = writeAt pos "O"
drawSlot (pos, FILLED) = writeAt pos "F"

drawSlots :: Slots -> [IO()]
drawSlots (s1, s2, s3) = [drawSlot s1, drawSlot s2, drawSlot s3]

drawFrame :: GameState -> IO()
drawFrame (p, ts, ss, _) =  do clearScreen
                               doIOActions (drawBackground ++ 
                                           (drawSlots ss) ++ 
                                           (drawTrucks ts) ++ 
                                           (drawPlayer p) ++ 
                                           [goto (width, height)])

charToMove :: Char -> Move
charToMove 'W' = UP
charToMove 'A' = LEFT
charToMove 'D' = RIGHT
charToMove 'S' = DOWN
charToMove 'Q' = QUIT
charToMove _ = NOTHING

applyMove :: Pos -> Move -> Pos
applyMove (x,y) UP = clampPos (x, y-1)
applyMove (x,y) DOWN = clampPos (x, y+1)
applyMove (x,y) RIGHT = clampPos (x+1, y)
applyMove (x,y) LEFT = clampPos (x-1, y)
applyMove (x,y) _ = (x,y)

clampPos :: Pos -> Pos
clampPos (x,y) = (clampBetween 1 width x, clampBetween 1 height y)

clampBetween :: Int -> Int -> Int -> Int
clampBetween min max val | val <= min = min
                         | val >= max = max
                         | otherwise = val


initialState :: GameState
initialState = (p, t, s, q)
                    where p = ((oneHalf, height), ALIVE)
                          t = [t1, t2, t3]
                          s =  (s1, s2, s3)
                          q = False
                          oneThird = width `div` 3
                          oneHalf = width `div` 2
                          s1 = ((oneThird, 1), EMPTY)
                          s2 = ((oneHalf, 1), EMPTY)
                          s3 = ((oneThird * 2, 1), EMPTY)
                          t1 = setTruckPos littleTruck (oneThird * 2, 10) 
                          t2 = setTruckPos bigTruck (oneThird * 2, 15)
                          t3 = setTruckPos mediumTruck (oneThird * 2, 20)

isOver :: GameState -> Bool
isOver (p, ts, s, q) = q

movePlayer :: Player -> Move -> Player
movePlayer (pos, ALIVE) m = (applyMove pos m, ALIVE)
movePlayer (pos, DEAD) m = (pos, DEAD)

simulateChar :: GameState -> Maybe Char -> GameState
simulateChar state (Just c) = simulateMove state (charToMove (toUpper c))
simulateChar state Nothing = simulateMove state NOTHING

simulateMove :: GameState -> Move -> GameState
simulateMove (p, ts, s, q) QUIT = (p, ts, s, True)
simulateMove (p, ts, s, q) move =   checkCollisionTrucks (movePlayer p move, map simulateTruck ts, s, q)

simulateTruck :: Truck -> Truck
simulateTruck ((x,y), size, speed) | (x - speed + size < 1) = setTruckPos bigTruck (width, y)
                                   | (x - speed < 1) = ((x,y), size - speed, speed) 
                                   | otherwise = ((x-speed, y), size, speed)

setDead :: Player -> Player
setDead (pos, state) = (pos, DEAD)

playerPosition :: Player -> Pos
playerPosition (pos, state) = pos

checkCollisionTrucks :: GameState -> GameState
checkCollisionTrucks (p, ts, s, q) =   if collision then (setDead p, ts, s, q) else (p, ts, s, q) 
                                        where
                                            collision = not (null [toTest | toTest <- allTruckPositions, toTest == playerPosition p])
                                            allTruckPositions = trucksPositions ts

trucksPositions :: [Truck] -> [Pos]
trucksPositions ts = concat (map truckPositions ts)

truckPositions :: Truck -> [Pos]
truckPositions ((x,y), len, sp) = take len [(x+num, y) | num <- [0..]] 


gameLoop state  | isOver state =  do clearScreen
                                     writeAt (0,0) "Goodbye!\n"
                | otherwise = do
                                c <- timeout inputTimeout getChar
                                clearScreen
                                let newState = simulateChar state c
                                drawFrame newState
                                gameLoop newState

frogger = do 
            drawFrame initialState
            gameLoop initialState

