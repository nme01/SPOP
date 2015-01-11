module Logic.SolutionFinder
( findSolution
) where

import Prelude hiding (Left,Right)
import Tanks
import Logic.Constraints

-- Finds solution of puzzle with given constraints and houes.
-- 1. list of constraints for rows
-- 2. list of constraints for columns
-- 3. list of coordinates (rowNr,colNr) of houses
-- out: list of TankPosition (nth position of tank
--            corresponds to nth house)
findSolution::[Int]->[Int]->[(Int,Int)]->[Move]
findSolution rowConstr colConstr houseCoords
    = findSolution' rowConstr colConstr houseCoords []

-- 1. list of row constraints
-- 2. list of columns constraints
-- 3. list of coordinates of houses
-- 4. partial solution
-- out: list of moves solving the puzzle
findSolution'::[Int]->[Int]->[(Int,Int)]->[Move]->[Move]
findSolution' rowConstr colConstr [] partResult =
    if hasSolutionErrors rowConstr colConstr [] partResult
    then
        []
    else
        partResult

findSolution' rowConstr colConstr houses partResult =
    if hasSolutionErrors rowConstr colConstr houses partResult
    then
        []
    else
            movesIter rowConstr colConstr restHouses partResult movesForHouse
        where
            movesForHouse = [(Move chosenHouse position) | position<-[Up,Down,Left,Right]]
            (chosenHouse,restHouses) = chooseHouse houses

-- Iterates over a list of moves available for the currently chosen house.
-- If no available moves are left, empty result is returned (meaning failure).
-- 1. row constraints
-- 2. column constraints
-- 3. houses unassigned
-- 4. partial result (list of "moves")
-- 5. moves available for the current house
-- out: list of moves solving the puzzle
--      or empty list if puzzle couldn't be using given partial result
movesIter::[Int]->[Int]->[(Int,Int)]->[Move]->[Move]->[Move]
movesIter _ _ _ _ [] = []
movesIter rowConstr colConstr houses partResult moves
        | isExistingSolution solution = solution
        | otherwise                   = movesIter rowConstr colConstr houses partResult restMoves
    where
        solution      = findSolution' newRowConstr newColConstr houses (chosenMove:partResult)
        newRowConstr  = updateRowConstr rowConstr chosenMove
        newColConstr  = updateColumnConstr colConstr chosenMove
        (chosenMove,restMoves) = chooseMove moves

-- Checks if given solution (sequence of moves solving the puzzle)
-- represents a existing solution or not.
-- 1. solution
-- out: True if given solution represents an existing solution,
--      False otherwise
isExistingSolution::[Move]->Bool
isExistingSolution [] = False
isExistingSolution _  = True

-- Chooses move to make from given list
-- for now: it always chooses first move from the list
-- 1. list of moves
-- out: a pair (chosenMove, restOfMoves). Where the chosenMove
--      is the chosen move and the restOfMoves is a given list
--      with chosenMove removed
chooseMove::[Move] -> (Move,[Move])
chooseMove (m:ms) = (m,ms)

-- Chooses next house to assign from the given list
-- for now: it always chooses the first house from the list
-- 1. list of houses without tanks assigned
-- out: a pair (chosenHouse, restOfHouses). Where the chosenHouse
--      is the chosen house and the restOfHouses is a given list
--      with chosenHouse removed
chooseHouse::[(Int,Int)] -> ((Int,Int),[(Int,Int)])
chooseHouse (h:hs) = (h,hs)

