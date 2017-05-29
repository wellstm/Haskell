-- Collection of Console IO Helpers
module ConsoleIO where
import System.IO
import Data.Char

type Pos = (Int, Int)

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

doIOActions :: [IO a] -> IO()
doIOActions [] = return ()
doIOActions (a:as) = do a
                        doIOActions as