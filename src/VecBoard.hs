module VecBoard
    ( Board
    , newBoard
    , showBoard
    , printBoard
    , hasLost
    , hasWon
    , dispatchCommand
    , Direction(..)
    , Command(..)
    ) where

import Data.List
import System.Random
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)

import qualified Data.Vector as V
import qualified Data.Set as S

data CellBomb = Bomb | NoBomb deriving (Eq, Show)
data CellState = Clicked | Unclicked | Marked deriving (Eq, Show)

type Position = (Int, Int)
type Cell = (CellBomb, CellState)
data Board = Board {table :: V.Vector (V.Vector Cell), width :: Int, height :: Int, 
                    position :: Position, hasLost :: Bool, hasWon :: Bool} deriving (Show)

replace :: (a -> Maybe a) -> Int -> V.Vector a -> Maybe (V.Vector a)
replace f n v = v V.!? n >>= f >>= \x -> return (v V.// [(n,x)])

replace2 :: (a -> Maybe a) -> Position -> V.Vector (V.Vector a) -> Maybe (V.Vector (V.Vector a))
replace2 f (x, y) xss = replace (replace f x) y xss

safeGetIndex :: Int -> V.Vector a -> Maybe a
safeGetIndex = flip (V.!?)

safeGetIndex2 :: Position -> V.Vector (V.Vector a) -> Maybe a
safeGetIndex2 (a,b) xss = safeGetIndex a =<< safeGetIndex b xss

zipPos :: V.Vector (V.Vector a) -> V.Vector (V.Vector (Position, a))
zipPos = V.map f . V.indexed . V.map V.indexed
           where f (y,vs) = V.map (\(x,v) -> ((x,y),v)) vs

surrounding :: Position -> [Position]
surrounding (x,y) = [(x-1,y-1), (x-1,y), (x-1,y+1),
                      (x, y-1),            (x,y+1),
                     (x+1,y-1), (x+1,y), (x+1,y+1)]
                  
nBombs :: Position -> Board -> Int
nBombs (x,y) b = length . filter (\c -> fst c == Bomb)
                        . mapMaybe (\p -> safeGetIndex2 p (table b))
                        $ surrounding (x,y)

frameRect :: [String] -> String
frameRect ss = let n = length (head ss)
                   topBottom = [replicate (n+2) '█']
               in intercalate "██\n██" (topBottom ++ ss ++ topBottom)

showBoard :: Board -> String
showBoard b = frameRect . V.toList . V.map (concat . V.toList . V.map padCell) . zipPos . table $ b
                where padCell (pos, c) | pos == position b = "<" ++ showCell pos c ++ ">"
                                       | otherwise = " " ++ showCell pos c ++ " "
                      showCell _ (Bomb, Clicked) = "X"
                      showCell _ (Bomb, Unclicked) | hasLost b = "X"
                      showCell _ (_, Unclicked) = "-"
                      showCell _ (_, Marked) = "#"
                      showCell pos (NoBomb, Clicked) | allClicked pos = " "
                      showCell pos (NoBomb, Clicked) | otherwise = show . flip nBombs b $ pos
                      allClicked (x,y) = null . filter (\c -> snd c /= Clicked)
                                       . mapMaybe (\p -> safeGetIndex2 p (table b))
                                       $ surrounding (x,y)

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard

newBoard :: RandomGen g => Int -> Int -> Double -> g -> Board
newBoard w h minePercent r = Board {table=t, width=w, height=h, position=(0,0),
                                       hasLost=False, hasWon=False}
    where t = V.fromList . map (V.fromList . map toCell) . take h . chunksOf w $ (randoms r :: [Double])
          toCell x = (if x < minePercent then Bomb else NoBomb, Unclicked)

updateBoardTable :: (Cell -> Maybe Cell) -> Board -> Maybe Board
updateBoardTable f board = replace2 f (position board) (table board) >>= \t ->
                                return (board {table = t})

hasBombExploded :: Board -> Bool
hasBombExploded =  V.any id . V.map (V.any exploded) . table
    where exploded (Bomb, Clicked) = True
          exploded (_, _) = False
          
-- everything must be either marked or clicked correctly
hasBoardCompleted :: Board -> Bool
hasBoardCompleted = not . V.any id . V.map (V.any wrong) . table
    where wrong (NoBomb, Unclicked) = True
          wrong (NoBomb, Marked) = True
          wrong (Bomb, Clicked) = True
          wrong (_, _) = False

-- the aux function takes a stack of positions to check and a set of positions
-- already checked. If a position has no bombs around it then the surrounding
-- positions are added to the stack, otherwise continue
clickPos :: Position -> Board -> Board
clickPos p b = let b' = clickPos' [p] (S.empty) b in
                    if hasBombExploded b' then b' {hasLost=True}
                    else if hasBoardCompleted b' then b' {hasWon=True}
                    else b'
    where clickPos' :: [Position] -> S.Set Position -> Board -> Board
          clickPos' [] _ b = b
          clickPos' (p:ps) done b = 
                case replace2 f p (table b) of
                    Nothing -> clickPos' ps (S.insert p done) b
                    Just t -> let b' = b {table = t} in
                              if nBombs p b' > 0 then clickPos' ps (S.insert p done) b'
                              else clickPos' (ps ++ newToCheck) (S.insert p done) b'
                                  where newToCheck = S.toList ((S.fromList . surrounding $ p) S.\\ done)
          f (c, Unclicked) = Just (c, Clicked)
          f c = Nothing

clickCell :: Board -> Board
clickCell b = clickPos (position b) b

toggleMarkCell :: Board -> Board
toggleMarkCell b = case updateBoardTable f b of
                Just b' -> b'
                Nothing -> b
             where f (c, Unclicked) = Just (c, Marked)
                   f (c, Marked) = Just (c, Unclicked)
                   f c = Nothing

updatePos :: Position -> Board -> Board
updatePos p@(x,y) b | x < 0 || x > maxX || y < 0 || y > maxY = b
                    | otherwise = b {position = p}
    where maxX = width b - 1
          maxY = height b - 1

fPos :: (Position -> Position) -> Board -> Board
fPos f b = updatePos (f . position $ b) b

data Direction = U | D | L | R deriving (Eq, Show)
data Command = ToggleMark | Click | Direction Direction deriving (Eq, Show)

movePos :: Direction -> Board -> Board
movePos U = fPos (\(x,y) -> (x,y-1))
movePos D = fPos (\(x,y) -> (x,y+1))
movePos L = fPos (\(x,y) -> (x-1,y))
movePos R = fPos (\(x,y) -> (x+1,y))

dispatchCommand :: Command -> Board -> Board
dispatchCommand ToggleMark = toggleMarkCell
dispatchCommand Click = clickCell
dispatchCommand (Direction d) = movePos d
