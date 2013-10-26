{-# LANGUAGE TypeOperators #-}
module WOC.Graphics.TerrainBuilder where

import WOC.Data.Collections
import Data.Vec as V
import Data.Monoid
import Control.Monad.Trans.Writer


type Quad a = (Vec3 a, Vec3 a, Vec3 a, Vec3 a)
type Rectangle a = (Vec3 a,  -- ^ Top left corner
                    Vec3 a)  -- ^ Bottom right corner
type Triangle a = (Vec3 a, Vec3 a, Vec3 a)

vec3 x y z = x:.y:.z:.()
getXY = V.take n2
getX = V.get n0
getY = V.get n1
getZ = V.get n1
-- ^ Would be better with lenses ;-)

-- | Z coordinate is that of the height!
tellTerrainRects ::
      (Monoid m) => Rectangle Double  -- ^ The rectangle containing the full terrain
                 -> (Rectangle a -> m)
                 -> QuadTree a
                 -> Int               -- ^ Desired level of detail
                 -> Writer m ()
tellTerrainRects fullMapRect toMonoid quadTree desiredLevel =
  recLevels quadTree fullMapRect 0 
  where recLevels (QTNode height tl tr bl br)
                  (rectTL, rectBR)
                  level
          | level == desiredLevel =
              undefined
          | otherwise = do
              undefined
          where halfDiagonalVec = ((getXY rectBR - getXY rectTL) / 2)
                                  `append` (getZ rectTL :. ())
                centerPos = rectTL + halfDiagonalVec

-- data MonoidMapper m a = MonoidMapper

