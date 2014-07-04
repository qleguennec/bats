module Util.Coord (
    coordItoD
    , multipleCoords
) where

coordItoD :: (Int, Int) -> (Double, Double)
coordItoD (x, y) = (fromIntegral x, fromIntegral y)

multipleCoords :: (Int, Int) -> Double -> (Double, Double)
multipleCoords xy a = let (x', y') = coordItoD xy in (a*x', a*y')
