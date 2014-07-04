import FRP.Helm
import FRP.Helm.Graphics (Element(..))
import qualified FRP.Helm.Window as Window

import Cell.Display (cellGridForm)
import LifeGame.Data.CellGrid (CellGrid(..), randCellGrid)

renderCellGrid :: CellGrid -> Double -> (Int, Int) -> Element
renderCellGrid cg sqr (x, y) = collage x y $ cellGridForm cg sqr

main :: IO ()
main = do
  cg <- randCellGrid 50 50
  run config $ renderCellGrid cg 10 <~ Window.dimensions
  where config = defaultConfig {windowTitle = "bats"
                               , windowDimensions = (500, 500)}
