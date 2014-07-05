import FRP.Helm
import FRP.Helm.Graphics (Element(..))
import qualified FRP.Helm.Window as Window
import FRP.Helm.Animation (Frame, AnimationStatus(..), animate, absolute)
import FRP.Helm.Time (second, running, delta)
import FRP.Elerea.Simple

import Cell.Display (allGenFrames)
import LifeGame.Data.CellGrid (CellGrid(..), randCellGrid)

render :: Form -> (Int, Int) -> Element
render form (x, y) = collage x y $ [form]

main :: IO ()
main = do
  cg   <- randCellGrid 50 50
  anim <- return . absolute $ allGenFrames cg (1 * second) 10
  engine <- startup defaultConfig

  run engine $ render <~ (animate anim running status) ~~ Window.dimensions engine

  where 
    config = defaultConfig { windowTitle = "bats"
                           , windowDimensions = (500, 500)}
    status = effectful $ getLine >>= \i -> return $
      case i of
        "Pause" -> Pause
        "Stop"  -> Stop
        "Cycle" -> Cycle
