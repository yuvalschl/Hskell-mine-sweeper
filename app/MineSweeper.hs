module MineSweeper where

import System.Random
import Data.Char
import Data.Maybe (isNothing)
import Data.List ((\\))

data GameState =  Won | Lost | Playing
    deriving (Read, Eq)

data Action = Dig Int Int | Flag Int Int
  deriving (Read,Eq)


data Board = Board {width :: Int
                   ,height :: Int
                   ,minesCount :: Int
                   ,matrix :: [[Cell]]
                   ,dugCells :: [Pos]
                   } deriving (Show)


data CellType = Mine | NoMine | Flagged | Around | Boom | NotFound
  deriving(Show,Eq)



data Cell = Cell {row :: Int
                  ,col :: Int
                  ,hiddenType :: CellType
                  ,shownType :: CellType
                  }deriving(Show,Eq)

type Pos = (Int,Int)

showGame :: Board -> GameState ->  String
showGame (Board width height minesCount matrix dugCells) state = "   " ++ makeFirstLine matrix width 0 ++ "\n" ++ showGameAux matrix matrix 0 state


showGameAux :: [[Cell]] -> [[Cell]] -> Int -> GameState -> String
showGameAux matrix [] _ _ = []
showGameAux matrix (x:xs) row state = initLine matrix (row + 1) x state ++ showGameAux matrix xs (row + 1) state



makeFirstLine :: [[Cell]] -> Int -> Int -> String
makeFirstLine matrix height toPrint
  |toPrint == height = []
  |toPrint < 9 = " 00" ++ show (toPrint + 1) ++ " " ++ makeFirstLine matrix height (toPrint + 1)
  |otherwise = " 0" ++ show (toPrint + 1) ++ " " ++ makeFirstLine matrix height (toPrint + 1)


initLine :: [[Cell]] -> Int -> [Cell] -> GameState -> String
initLine matrix row cell state
  |row < 10 = "00" ++ show row ++ makeLine matrix cell state ++ "\n"
  |otherwise = "0" ++ show row ++ makeLine matrix cell state ++ "\n"


makeLine :: [[Cell]] -> [Cell] -> GameState -> String
makeLine _ [] _ = []
makeLine matrix (x:xs) state
  |hiddenType x == Mine && state == Lost = " [*] " ++ makeLine matrix xs state
  |shownType x == Flagged = " [!] " ++ makeLine matrix xs state
  |shownType x == Around = " [" ++ [minesAround matrix pos] ++ "] " ++ makeLine matrix xs state
  |shownType x == NoMine = " [ ] " ++ makeLine matrix xs state
  |otherwise = " [ ] " ++ makeLine matrix xs state
  where pos = (row x + 1, col x + 1)


randomCoord :: Int -> Int -> StdGen -> (Pos, StdGen)
randomCoord width height gen =
  let (r1, gen') = randomR (0, width - 1) gen
      (r2, gen'') = randomR (0, height - 1) gen'
  in ((r1, r2), gen'')


randomCoordExcept :: [Pos] -> Int -> Int -> StdGen -> (Pos, StdGen)
randomCoordExcept ts width height gen =
  let (t, gen') = randomCoord width height gen
   in if t `elem` ts
        then randomCoordExcept ts width height gen'
        else (t, gen')


generateMines :: [Pos] -> Int -> Int -> Int -> StdGen -> ([Pos], StdGen)
generateMines ts width height 0 gen = ([], gen)
generateMines es width height numOfMines gen =
   let (t, gen') = randomCoordExcept es width height gen
       (ts, gen'') = generateMines (t : es) width height (numOfMines - 1) gen'
   in (t : ts, gen'')


newGame :: [Pos] -> Int-> Int-> Int -> StdGen -> Board
newGame minesPosList width height numOfMines gen = Board width height numOfMines cells []
  where
    cells = initCells minesPosList width height
      where (minesPosList, gen'') = generateMines [] width height numOfMines gen


initCells :: [Pos] -> Int -> Int -> [[Cell]]
initCells minesList width height  = [[if (x,y) `elem` minesList then Cell x y Mine Mine else Cell x y NoMine NoMine | x <- [0..width - 1] ] | y <- [0..height - 1]]


{-//////////////////////////////////////////// GAMEPLAY //////////////////////////////////////////-}

getCell :: [[Cell]] -> Pos -> Cell
getCell [] pos = Cell 99 99 NotFound NotFound
getCell (x:xs) pos
  | shownType cell == NotFound = getCell xs pos
  | otherwise = cell
  where cell = getCellAux x pos

getCellAux :: [Cell] -> Pos -> Cell
getCellAux [] pos = Cell 99 99 NotFound NotFound
getCellAux (c:cs) (x,y)
  |row c == (x - 1) && col c == (y - 1) = c
  |otherwise = getCellAux cs (x,y)


dig :: Board -> Pos -> Board
dig (Board width height minesCount matrix dugCells) (x, y)
  | celltype == Mine =
    Board width height minesCount (updateBoard (Board width height minesCount matrix dugCells) (x, y) Boom) dugCells
  | celltype == NoMine =
    Board width height minesCount (updateBoard (Board width height minesCount matrix dugCells) (x, y) Around) ((x,y) : dugCells)
  | otherwise = Board width height minesCount matrix dugCells {-Pos is Flagged or dug-}
    where celltype = hiddenType (getCell matrix (x,y))


toggleFlag :: Board -> Pos -> Board
toggleFlag (Board width height minesCount matrix dugCells) (x, y)
  | celltype == Flagged =
    Board width height minesCount (updateBoard (Board width height minesCount matrix dugCells) (x, y) NoMine) dugCells
  |celltype == Around =
    Board width height minesCount (updateBoard (Board width height minesCount matrix dugCells) (x, y) Around) dugCells
  | otherwise = Board width height minesCount (updateBoard (Board width height minesCount matrix dugCells) (x, y) Flagged) dugCells
  where celltype = shownType (getCell matrix (x,y))


updateBoard :: Board -> Pos -> CellType -> [[Cell]] {-update the board after dig or toggle flag, get the current board state
 pos to update and celltype to put in the pos -}
updateBoard (Board width height minesCount [] dugCells) pos celltype = []
updateBoard (Board width height minesCount (x:xs) dugCells) pos celltype =
  updateLine x pos celltype : updateBoard (Board width height minesCount xs dugCells) pos celltype


updateLine :: [Cell] -> Pos -> CellType -> [Cell]
updateLine [] pos cell = []
updateLine (c:cs) (x, y) celltype
  | row c == (x - 1) && col c == (y - 1) = Cell (x - 1) (y - 1) (hiddenType c) celltype  : updateLine cs (x, y) celltype
  | otherwise = c : updateLine cs (x, y) celltype


act :: Board -> Action -> Board
act board (Dig x y)
 |celltype == Flagged = board
 | x < 1 || x > width board = dig board (x,y)
 | y < 1 || y > height board = dig board (x,y)
 | minesAround (matrix board) (x,y) /= '0' = dig board (x,y)
 | minesAround (matrix board) (x,y) == '0' && notElem (x,y) (dugCells board)  = foldl act (dig board (x,y)) toVisit
 | otherwise = board
 where toVisit =  [(Dig (x+dx) (y+dy)) | dx <- [-1..1], dy <- [-1..1] , dx /= 0 || dy /=0]
       celltype = shownType (getCell (matrix board) (x,y))
act board (Flag x y) = toggleFlag board (x,y)


minesAround :: [[Cell]] -> Pos -> Char
minesAround matrix (x,y) = intToDigit (length $ filter (\c -> hiddenType c == Mine)
 [getCell matrix (x + dx, y + dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0])


validateArgs :: [String] -> Bool
validateArgs args
  | width > 20 || width < 10 = False
  | height > 20 || height < 10 = False
  | minesCount < 4 || minesCount > (width * height) - 1  || minesCount > 199 = False
  | otherwise = True
  where
    width = read $ head args :: Int
    height = read $ args !! 1 :: Int
    minesCount = read $ args !! 2 :: Int

validateTypes :: [Maybe Int] -> Bool
validateTypes = elem Nothing

determineAction :: String -> Int -> Int -> Action
determineAction action row col
  |action == "dig" = Dig row col
  |otherwise = Flag row col


keepPlaying :: Board -> Bool
keepPlaying (Board width height minesCount matrix dugCells)
  |isBoom matrix = False
  |countDugCells matrix == width * height - minesCount = False
  |otherwise = True


isBoom:: [[Cell]] -> Bool
isBoom [] = False
isBoom (x:xs)
  |isBoomHelper x = True
  |otherwise = isBoom xs


isBoomHelper:: [Cell] -> Bool
isBoomHelper [] = False
isBoomHelper (x:xs)
  |shownType x == Boom = True
  |otherwise = isBoomHelper xs


getState :: Board -> GameState
getState board
  |keepPlaying board = Won
  |otherwise = Lost


countDugCells :: [[Cell]] -> Int
countDugCells = foldr ((+) . countDugCellsHelper) 0

countDugCellsHelper :: [Cell] -> Int
countDugCellsHelper [] = 0
countDugCellsHelper (x:xs)
  |shownType x == Around = 1 + countDugCellsHelper xs
  |otherwise = 0 + countDugCellsHelper xs

validateActionType :: Maybe Int -> Maybe Int -> Bool
validateActionType _width _height
  | isNothing _width = False
  | isNothing _height = False
  |otherwise = True

validateAction :: String -> Int -> Int -> Int -> Int -> Bool
validateAction _action _width _height boardwidth boardheight
  | _action /= "dig" && _action /= "flag" = False
  | _width > boardwidth || _width < 1 = False
  | _height > boardheight || _height < 1 = False
  |otherwise = True
