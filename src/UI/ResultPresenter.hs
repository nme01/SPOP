module UI.ResultPresenter
( presentResult
) where

import Tanks
import UI.ResultPrinter
import System.IO

import Prelude hiding (Left,Right)

-- Presents the solved puzzle and saves the solution
-- to file. The file is specified by user.
-- 1. rows constraints
-- 2. columns constraints
-- 3. list of moves necessary to solve the puzzle
-- out: IO
presentResult::[Int]->[Int]->[(Int,Int)]->[Move]->IO()
presentResult rowsConstr columnsConstr houses moves =
    do printSolution rowsConstr columnsConstr houses moves
       saveStateToFile rowsConstr columnsConstr houses moves

-- Saves the puzzle and it's solution to file given by user.
-- 1. row constraints.
-- 2. column constraints.
-- 3. coordinates of houses.
-- 4. sequence of moves solving the puzzle.
-- out: IO
saveStateToFile:: [Int]->[Int]->[(Int,Int)]->[Move]->IO()
saveStateToFile rowsConstr columnsConstr houses moves =
    do  fileName <- readFileName
        outh <- openFile fileName WriteMode
        hPrint outh rowsConstr
        hPrint outh columnsConstr
        hPrint outh houses
        hPrint outh moves
        hClose outh

-- Gets the name of result file from user.
readFileName::IO String
readFileName = do putStrLn "Output file name: "
                  fileName <- getLine
                  return fileName
