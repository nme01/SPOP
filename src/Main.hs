module Main where

import Logic.SolutionFinder
import UI.ResultPresenter
import UI.PuzzleLoader

main :: IO()
main = do
       (rowConstr, colConstr, houses) <- loadPuzzle
       let solution = findSolution rowConstr colConstr houses
       presentResult rowConstr colConstr houses solution
