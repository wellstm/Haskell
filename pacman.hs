import System.IO
import Data.Char

type Pos = (Int, Int)
data Cell = Empty | Pellet | Player
data GameAction = UP | DOWN | LEFT | RIGHT | NOTHING | QUIT
type Row = [Cell]
type Board = [Row]

width :: Int
width = 10

height :: Int
height = 10

colSpacing :: String
colSpacing = "   "

beep :: IO()
beep = putStr "\BEL"

clearScreen :: IO()
clearScreen = do putStr "\ESC[2J"
                 goto (0,0)

goto :: Pos -> IO()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO()
writeAt p msg = do goto p
                   putStr msg

seqn :: [IO a] -> IO()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

isMiddle :: Pos -> Bool
isMiddle (x,y) = x == width `div` 2 && y == height `div` 2

generateInitialRow :: Int -> Row
generateInitialRow y = [if isMiddle(x,y) then Player else Pellet | x <- [1..width]]


initialBoard :: Board
initialBoard = [generateInitialRow y | y <- [1..height]]

cellToChar :: Cell -> Char
cellToChar Empty = ' '
cellToChar Pellet = '*'
cellToChar Player = 'O'

charToAction :: Char -> GameAction
charToAction 'W' = UP
charToAction 'A' = LEFT
charToAction 'S' = DOWN
charToAction 'D' = RIGHT
charToAction 'Q' = QUIT
charToAction _ = NOTHING

rowToString :: Row -> String
rowToString row = concat [(cellToChar cell) : colSpacing | cell <- row]

drawBoard :: Board -> IO()
drawBoard board = seqn [putStrLn (rowToString row) | row <- board]


isOver :: Board -> Bool
isOver b = null b

simulate :: Board -> GameAction -> Board
simulate b QUIT = []
simulate b NOTHING = b
simulate b a =  setCellTo newBoard Player newPosition
                where oldPosition = findPlayer b
                      newBoard = setCellTo b Empty oldPosition
                      newPosition = newPlayerPosition a oldPosition


setCellTo :: Board -> Cell -> Pos -> Board
setCellTo b c (x,y) = [if rowNum == y then setCellInRow row x c else row | (row, rowNum) <- zip b [1..width]]

setCellInRow :: Row -> Int -> Cell -> Row
setCellInRow row pos value = [ if cellNum == pos then value else cell | (cell, cellNum) <- zip row [1..width]]


newPlayerPosition :: GameAction -> Pos -> Pos
newPlayerPosition UP (x,y) = if y <= 1 then (x,1) else (x, y-1)
newPlayerPosition DOWN (x,y) = if y >= height then (x,height) else (x, y+1)
newPlayerPosition LEFT (x,y) = if x <= 1 then (1,y) else (x-1, y)
newPlayerPosition RIGHT (x,y) = if x >= width then (width,y) else (x+1, y)
newPlayerPosition _ pos = pos

findPlayer :: Board -> Pos
findPlayer board = (x,y)
                   where x = findPlayerInRow row
                         (row, y) =  findRowWithPlayer board

findRowWithPlayer :: Board -> (Row, Int)
findRowWithPlayer board = head [(row, rowNum) | (row, rowNum) <- zip board [1..height], isPlayerInRow row ]

isPlayerInRow :: Row -> Bool
isPlayerInRow row = not (null (filter isPlayer row))

findPlayerInRow :: Row -> Int
findPlayerInRow row =  head [cellNum | (cell, cellNum) <- zip row [1..width], isPlayer cell]

isPlayer :: Cell -> Bool
isPlayer Player = True
isPlayer _ = False

gameLoop board  | isOver board =  do beep
                | otherwise = do 
                                x <- getChar
                                clearScreen
                                let newboard = (simulate board (charToAction (toUpper x)))
                                drawBoard newboard
                                gameLoop newboard

pacman = do 
            putStrLn "Press any key to start...."
            gameLoop initialBoard