module Iter.Iter where

import Iter.Rules (ruler)
import Data.CellGrid (CellGrid(..))

next :: CellGrid -> CellGrid
next cg@(CellGrid rc xs) = CellGrid rc $ map (ruler cg) xs
