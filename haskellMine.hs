module Main where 

import System.Console.ANSI (clearScreen, setTitle)
import Data.List (lookup, null)
import System.Random
import Text.Read (readMaybe)
import System.Console.Haskeline (runInputT, defaultSettings, getInputChar)
import Board

defaultRows = 20
defaultCols = 20
defaultProb = 0.1

question :: String -> String -> String
question q def = q ++ " do you want? (Default=" ++ def ++ ")"

main = do
    setTitle "Minesweeper!"
    g <- getStdGen
    putStrLn "Welcome!"
    putStrLn $ question "How many rows" (show defaultRows)
    r <- getLine
    putStrLn $ question "How many columns" (show defaultCols)
    c <- getLine
    putStrLn $ question "What prob of mines" (show defaultProb)
    p <- getLine
    let rows = case (readMaybe r :: Maybe Int) of
                Just n -> n
                Nothing -> defaultRows
        cols = case (readMaybe c :: Maybe Int) of
                Just n -> n
                Nothing -> defaultCols
        prob = case (readMaybe p :: Maybe Double) of
                Just n -> n
                Nothing -> defaultProb
    playGame (newBoard rows cols prob g)
    
keyMap :: [(Char, Command)]
keyMap = [('w', Direction U),
          ('a', Direction L),
          ('s', Direction D),
          ('d', Direction R),
          ('m', ToggleMark),
          (' ', Click)]
          
executeKey :: Char -> Board -> Board
executeKey c b = case lookup c keyMap of
                            Nothing -> b
                            Just comm -> dispatchCommand comm b

exit :: Char -> Bool
exit c = c `elem` ['Q', 'q']

pullChar :: IO (Char)
pullChar = (runInputT defaultSettings $ getInputChar "") >>= (\c -> 
              case c of
                 Just x -> return x
                 Nothing -> error "Error receiving character")

showGame :: Board -> IO ()
showGame b = do
    clearScreen
    printBoard b
    putStrLn "Direction: w,a,s,d\nClick: space\nToggle mark: m\nExit: Q"

playGame :: Board -> IO ()
playGame b = do
    showGame b
    command <- pullChar
    if (exit command) then putStrLn "Sure you want to leave? (Y)"
                                >> pullChar >>= (\i -> 
                                    if (i `elem` ['Y', 'y']) 
                                    then return () else playGame b)
    else let b' = executeKey command b in
        if (hasLost b') then showGame b' >> putStrLn "Whoops! Hit a mine" >> return ()
        else if (hasWon b') then showGame b' >> putStrLn "Yay! You won" >> return ()
        else playGame b'
    
