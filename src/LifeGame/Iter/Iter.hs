module LifeGame.Iter.Iter where

import LifeGame.Iter.Rules (ruler)
import LifeGame.Data.CellGrid (CellGrid(..))

next :: CellGrid -> CellGrid
next cg@(CellGrid rc xs) = CellGrid rc $ map (ruler cg) xs
