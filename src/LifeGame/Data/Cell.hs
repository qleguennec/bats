{-# LANGUAGE TypeFamilies #-}

module LifeGame.Data.Cell (
) where

import Data.Maybe (catMaybes)

import Math.Geometry.Grid
import Math.Geometry.Grid.SquareInternal (rectSquareGrid, SquareDirection)

data State = Alive | Dead
 deriving (Eq, Show)

data Cell = Cell State (Int, Int)
 deriving (Eq)

instance Show Cell where
 show (Cell s ind) = (show s) ++ " " ++ (show ind)

data CellGrid = CellGrid (Int, Int) [Cell]
 deriving (Eq)

cellGrid :: Int -> Int -> State -> CellGrid
cellGrid r c s = CellGrid (r, c) $ [Cell s (x, y) | x <- [0..c-1], y <- [0..r-1]]

searchCell :: CellGrid -> (Int, Int) -> Maybe Cell
searchCell (CellGrid _ cs) i = case filter (\(Cell s ind) -> ind == i) cs of
 [] -> Nothing
 xs -> Just . head $ xs

instance Show CellGrid where
 show (CellGrid (r, c) _) = "CellGrid " ++ (show r) ++ " " ++ (show c)

instance Grid CellGrid where
 type Index CellGrid = Cell
 type Direction CellGrid = SquareDirection

 indices (CellGrid _ xs) = xs
 neighbours cg (Cell _ (x, y)) = catMaybes [ 
    searchCell cg (x, y+1)
  , searchCell cg (x, y-1)
  , searchCell cg (x+1, y)
  , searchCell cg (x-1, y)]
 distance _ (Cell _ (x1, y1)) (Cell _ (x2, y2)) = abs (x2-x1) + abs (y2-y1)
 directionTo (CellGrid (r, c) _) (Cell _ (x1, y1)) (Cell _ (x2, y2)) = directionTo (rectSquareGrid r c) (x1, y1) (x2, y2)
