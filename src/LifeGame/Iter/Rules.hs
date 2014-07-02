module LifeGame.Iter.Rules where

import LifeGame.Data.Cell
import LifeGame.Data.CellGrid

ruler :: CellGrid -> Cell -> Cell
ruler cg
    | nAlives == 3 = birth
    | nAlives < 2 || nAlives > 3 = death
 where nAlives = length . alives $ cg
