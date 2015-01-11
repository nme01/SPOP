module UI.PuzzleLoader
( loadPuzzle
) where

import System.IO
import Text.Read

-- Loads all data defining the puzzle.
-- returns IO (rowConstraints, columnConstraints, housesCoordinates)
loadPuzzle::IO ([Int],[Int],[(Int,Int)])
loadPuzzle =
    do  fileName <- readFileName
        inh <- openFile fileName ReadMode
        rowConstraintsLine <- hGetLine inh
        columnConstraintsLine <- hGetLine inh
        housesLine <- hGetLine inh
        hClose inh
        let rowConstraints = read rowConstraintsLine ::[Int]
        let columnConstraints = read columnConstraintsLine ::[Int]
        let houses = read housesLine ::[(Int,Int)]
        return (rowConstraints,columnConstraints,houses)

-- Reads the input file's name.
readFileName::IO String
readFileName = do putStrLn "Input file name: "
                  fileName <- getLine
                  return fileName
