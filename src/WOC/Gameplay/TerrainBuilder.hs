module WOC.Gameplay.TerrainBuilder where

import WOC.Data.Collections
import WOC.Data.Math


-- TODO: Not a constant
bestLevel :: Double
bestLevel = 70

buildTerrainQT :: Integral a => (a -> Double) -> a -> Double -> RandomStateM (QuadTree Double)
buildTerrainQT getMeanHeight curLevel lastElevation = do
  rx <- randomM
  let meanHeight = getMeanHeight curLevel  -- getMeanHeight must be decreasing (not necessarily strictly)
      applyElevation = lastElevation /= 0 ||
                       rx < halfGaussians (bestLevel*0.5) bestLevel (bestLevel*0.15)
                                          (fromIntegral curLevel)
  curElevation <- if applyElevation
    then fst `fmap` randomNormalLaw meanHeight (meanHeight/8)
    else return lastElevation
  let genNextNode = buildTerrainQT getMeanHeight (curLevel+1) (max 0 curElevation)
  return (QTNode curElevation)
    `splitM` genNextNode
    `splitM` genNextNode
    `splitM` genNextNode
    `splitM` genNextNode

