module UI.ResultPrinter
( printSolution
) where

import Prelude hiding (Left,Right)
import Tanks

--Prints solution (sequence of moves to solve the puzzle)
--on the board of given size
--1. rows constraints
--2. columns constraints
--3. coordinates of houses
--4. solution
--out. solution printed on the screen
printSolution::[Int]->[Int]->[(Int,Int)]->[Move]->IO()
printSolution rowsConstr columnsConstr houses moves
        = printBoard board
    where
        board        = putTanksIntoBoard moves initialBoard
        initialBoard = putHouses houses emptyBoard
        emptyBoard   = initBoard rowsCount columnsCount
        rowsCount    = length rowsConstr
        columnsCount = length columnsConstr

--Prints the given board on the screen.
--1. 2-dimensional array containing characters representing
--   houses and tanks.
--out. board printed on the screen
printBoard::[[Char]] -> IO ()
printBoard xs = sequence_ [printRow x | x<-xs]

--Prints a single row.
printRow::[Char] -> IO ()
printRow xs = putStrLn [ x | x<-xs]

--Put tanks into the given board (2-dim array).
putTanksIntoBoard::[Move]->[[Char]]->[[Char]]
putTanksIntoBoard [] partialBoard = partialBoard
putTanksIntoBoard (m:moves) partialBoard
        = putTanksIntoBoard moves newBoard
    where
        newBoard = putTank m partialBoard

--Puts a single tank into board
putTank::Move->[[Char]]->[[Char]]
putTank move board
        = putCharIntoBoard (tankChar move) (moveToCoordinates move) board

--Converts a tank into a character representing it.
tankChar::Move->Char
tankChar (Move _ position)
    | position == Up    = tankAbove
    | position == Down  = tankUnder
    | position == Left  = tankToTheLeft
    | position == Right = tankToTheRight

--Puts houses into the given board (2-dim array).
putHouses::[(Int,Int)]->[[Char]]->[[Char]]
putHouses [] board = board
putHouses ((x,y):hs) board
        = putHouses hs newBoard
    where
        newBoard = putCharIntoBoard houseChar (x,y) board

--Places a given character on given coordinates in the board.
putCharIntoBoard::Char->(Int,Int)->[[Char]]->[[Char]]
putCharIntoBoard symbol (x,y) board
    = boardsInitPart ++ modifiedRow:boardsTailPart
    where
        (boardsInitPart,replacedRow:boardsTailPart) = splitAt x board
        modifiedRow = rowsInitPart ++ symbol:rowsTailPart
        (rowsInitPart,_:rowsTailPart) = splitAt y replacedRow

-- Definitions of characters corresponding to different fields
emptyField::Char
tankUnder::Char
tankAbove::Char
tankToTheRight::Char
tankToTheLeft::Char
houseChar::Char

emptyField = ' '
tankUnder = '^'
tankAbove = 'v'
tankToTheRight = '<'
tankToTheLeft = '>'
houseChar = 'H'

--Creates an empty board of given size
--1. number of rows
--2. number of columns
initBoard::Int->Int->[[Char]]
initBoard rowsCount columnsCount =
    if rowsCount <= 0 || columnsCount <= 0
    then
        error "Rows count and columns count must have positive values"
    else
        [[emptyField | _<-[1..columnsCount]] | _<-[1..rowsCount]]
