module Tanks
( Move (Move)
 ,TankPosition (Up,Down,Left,Right)
 ,moveToCoordinates
) where

import Prelude hiding (Left,Right)

-- Enum representing the position of a tank in relation to house
-- it belongs to.
data TankPosition = Up | Down | Left | Right deriving (Enum,Eq)

instance Show TankPosition where
    show Up    = "U"
    show Down  = "D"
    show Left  = "L"
    show Right = "R"

-- Data type representing a move to make in a puzzle.
-- It consists of houses coordinates and its tank's position.
-- Eg. Move (0,0) Right represents a move in which for house
-- of coordinates (0,0) tank is added on the right.
data Move = Move (Int,Int) TankPosition

instance Show Move where
    show (Move (x,y) position) = "(" ++ (show x) ++ "," ++ (show y) ++ ")" ++ (show position)

-- Extracts tanks's coordinates from Move.
moveToCoordinates::Move->(Int,Int)
moveToCoordinates (Move (x,y) position)
    | position == Up    = (x-1,y)
    | position == Down  = (x+1,y)
    | position == Left  = (x,y-1)
    | position == Right = (x,y+1)


