module Cell.Display (
    cellForm
    , cellGridForm
    , allGenFrames
) where

import Util.Coord (multipleCoords)

import LifeGame.Data.Cell (Cell(..), State(..))
import LifeGame.Data.CellGrid (CellGrid(..))
import LifeGame.Gen (Gen(..), gens)

import FRP.Helm.Graphics (Form(..), move, filled, square, group)
import FRP.Helm.Color (Color(..), white, black)
import FRP.Helm.Animation (Frame(..), Animation, absolute)
import FRP.Helm.Time (Time, millisecond)

-- returns corresponding Color
stToColor :: State -> Color
stToColor s = case s of
  Alive -> white
  Dead  -> black

-- Given a Cell and a square area, returns a square
-- representing the cell
cellForm :: Cell -> Double -> Form
cellForm (Cell s xy) sqr = let coords = xy `multipleCoords` sqr in
  move coords .  filled (stToColor s) $ square sqr

-- applies cellFrom all along a CellGrid
cellGridForm :: CellGrid -> Double -> [Form]
cellGridForm (CellGrid _ cs) sqr = map (\c -> cellForm c sqr) cs

-- For a given Gen, a square area and a time (the amount of time beetween frames),
-- returns a Frame composed of all the frames of the Cells with the same time
genFrames :: Gen -> Double -> Double -> Frame
genFrames (Gen t cg) sqr t' = (absoluteT, group $ cellGridForm cg sqr)
  where absoluteT = (fromIntegral t) + t'

-- allGenFrames takes a CellGrid, a square area and the time beetween CellGrid Frames
-- and returns an animation of all the generations of the CellGrid
allGenFrames :: CellGrid -> Double -> Double -> Animation
allGenFrames cg sqr t = map (\gen -> genFrames gen sqr t) $ gens cg
