{-# LANGUAGE BangPatterns, DeriveFunctor #-}
module WOC.Gameplay.TerrainBuilder where

import Data.Vec
import qualified Data.Vector as V
import Data.List (foldl')
import System.Random
import qualified Control.Monad.Trans.State as St
import Data.Function (fix)


data QuadTree a = QTNode { qtNodeValue :: a
                         , qtTopLeft :: QuadTree a
                         , qtTopRight :: QuadTree a
                         , qtBtmLeft :: QuadTree a
                         , qtBtmRight :: QuadTree a }
  deriving (Functor)
              

gaussian :: Double -> Double -> Double -> Double
gaussian mean sigma x = (1 / (sigma * sqrt (2*pi))) * exp (- (x - mean)^^2 / 2 * sigma^^2)
-- largeur à mi-hauteur = 2.3548 * sigma

data Inclusivity = Incl | Excl
excl n = (Excl, n)
incl n = (Incl, n)
x `inside` ((ia,a),(ib,b)) = x > a && fun ia x a && x < b && fun ib x b
  where fun Incl = (==)
        fun Excl = (/=)
x `outside` i = not (x `inside` i)

-- | Two half-gaussians spread around mean
halfGaussians :: Double -> Double -> Double -> Double -> Double
halfGaussians sigmaBefore mean sigmaAfter x =
  let s | x <= mean = sigmaBefore
        | otherwise = sigmaAfter
  in gaussian mean s x

average l = tot/len
  where (tot, len) = foldl' (\(!t,!l) !x -> (t+x,l+1)) (0,0) l

randomM = do gen <- St.get
             let (x,gen') = random gen
             St.put gen'
             return x
a1 `splitM` a2 = do gen <- St.get
                    let (g1,g2) = split gen
                        (g3,g4) = split g1
                    f <- St.withState (const g2) a1
                    x <- St.withState (const g3) a2
                    St.put g4
                    return (f x)

-- | Using Box-Muller method to generate two independent vars
-- following normal law
randomNormalLaw :: Double -> Double -> St.State StdGen (Double, Double)
randomNormalLaw mean sigma = do
  (u1, u2) <- fix $ \retry -> do
    u1 <- randomM
    u2 <- randomM
    if u1 == 0 || u2 == 0
      then retry
      else return (u1, u2)
  let coord f = mean + sigma * sqrt (-2 * log u1) * f (2 * pi * u2)
  return (coord cos, coord sin)

-- TODO: Not a constant
bestLevel = 70

buildTerrainQT :: Integral a => (a -> Double) -> a -> Double -> St.State StdGen (QuadTree Double)
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

