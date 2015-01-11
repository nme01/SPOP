module Logic.Constraints (
    updateRowConstr
   ,updateColumnConstr
   ,hasSolutionErrors
) where

import Prelude hiding (Left,Right)
import Tanks

-- Checks if last move was invalid. In order to check this
-- row and column constraints are checked as well
-- as neighbourhood of new tank (no tanks in neighbourhood
-- are allowed and it cannot collide with any house).
-- 1. row constraints
-- 2. column constraints
-- 3. coordinates of houses
-- 4. tanks already placed
-- out: True if last move was invalid, False otherwise
hasSolutionErrors::[Int]->[Int]->[(Int,Int)]->[Move]->Bool
hasSolutionErrors _ _ _ [] = False
hasSolutionErrors rowConstr colConstr houses (lastMove:moves)
      =    constraintsViolated rowConstr
        || constraintsViolated colConstr
        || isTankColliding moveCoords houses moves
        || isTankOutOfBoard moveCoords maxX maxY
    where
        maxX = (length rowConstr)-1
        maxY = (length colConstr)-1
        moveCoords = moveToCoordinates lastMove

-- Checcks if row/column constraints are violated (if they
-- contain any negative value).
-- 1. list of constraints
-- out: True if one of constraints has been violated, False otherwise
constraintsViolated::[Int]->Bool
constraintsViolated constraints = any (<0) constraints

-- Updates row constraints after making a given move.
-- 1. row constraints
-- 2. move
-- out: updated row constraints
updateRowConstr::[Int]->Move->[Int]
updateRowConstr constrList (Move (x,_) position)
     | position == Up    = decrementAt constrList (x-1)
     | position == Down  = decrementAt constrList (x+1)
     | otherwise         = decrementAt constrList x

-- Updates column constraints after making a given move.
-- 1. column constraints
-- 2. move
-- out: updated row constraints
updateColumnConstr::[Int]->Move->[Int]
updateColumnConstr constrList (Move (_,y) position)
  | position == Left  = decrementAt constrList (y-1)
  | position == Right = decrementAt constrList (y+1)
  | otherwise         = decrementAt constrList y

-- Decrements number of given index in the list. If given index
-- is out of bounds unchanged list is returned.
-- 1. list of Integers
-- 2. index of an element to decrement (first one is 0)
-- out: updated row constraints
decrementAt::[Int]->Int->[Int]
decrementAt [] _    = []
decrementAt (x:xs) k | k<0       = (x:xs)
                     | k==0      = ((x-1):xs)
                     | otherwise = (x:(decrementAt xs (k-1)))

-- checks if given move (tank placement) is colliding with other tanks
-- or houses. Returns true if new tank would collide with something.
-- 1. move to make (tank placement)
-- 2. list of coordinates of houses
-- 3. list of tanks already placed
-- out: True if tank collides with any house or tank
isTankColliding::(Int,Int)->[(Int,Int)]->[Move]->Bool
isTankColliding tankCoords houses tanks
    = contains houses tankCoords || collidesWithTank tankCoords tanks

-- checks if tank of given coordinates is out of board
-- 1. tank's coordinates
-- 2. max x value
-- 3. max y value
-- out: True if tank is out of board, False otherwise
isTankOutOfBoard::(Int,Int)->Int->Int->Bool
isTankOutOfBoard (x,y) maxX maxY
        = x>maxX || y>maxY || x<0 || y<0

-- checks if list contains a given element
-- 1. list of elements
-- 2. element searched for
-- out: True if list contains this element, False otherwise
contains::Eq a => [a]->a->Bool
contains elements a = any (a==) elements

-- checks if any tank on the list collides with the given tank
-- 1. coordinates of tank
-- 2. list of tanks
-- out: True if tank collides with any tank from the list,
--      False otherwise
collidesWithTank::(Int,Int)->[Move]->Bool
collidesWithTank newMove moves = any(areNeighbours newMove) moves

-- checks if two tanks are neighbours
-- 1. coordinates of first tank
-- 2. second tank
-- out: True if tanks are neighbours, False otherwise
areNeighbours::(Int,Int)->Move->Bool
areNeighbours (x_a,y_a) m =
        abs (x_b - x_a) <= 1 && abs (y_b - y_a) <= 1
    where
        (x_b,y_b) = moveToCoordinates m