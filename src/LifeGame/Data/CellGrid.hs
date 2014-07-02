{-# LANGUAGE TypeFamilies #-}

module Data.CellGrid where

import Data.Cell
import Data.Maybe (catMaybes)

import Math.Geometry.Grid
import Math.Geometry.Grid.SquareInternal (RectSquareGrid(..), SquareDirection)

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
 directionTo (CellGrid rc cs) (Cell _ xy1) (Cell _ xy2) = (\rc -> directionTo rc xy1 xy2) . RectSquareGrid rc . map (\(Cell _ xy) -> xy) $  cs
 
alives :: CellGrid -> [Cell]
alives = filter (isAlive) . indices

deads :: CellGrid -> [Cell]
deads = filter (isDead) . indices
