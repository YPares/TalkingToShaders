{-# LANGUAGE BangPatterns #-}
module WOC.Data.Math where

import System.Random
import qualified Control.Monad.Trans.State as St
import Data.List (foldl')
import Data.Function (fix)


gaussian :: Double -> Double -> Double -> Double
gaussian mean sigma x = (1 / (sigma * sqrt (2*pi))) * exp (- (x - mean)^^2 / 2 * sigma^^2)
-- largeur à mi-hauteur = 2.3548 * sigma

data Inclusivity = Incl | Excl
excl :: t -> (Inclusivity, t)
excl n = (Excl, n)
incl :: t -> (Inclusivity, t)
incl n = (Incl, n)
inside :: Ord a => a -> ((Inclusivity, a), (Inclusivity, a)) -> Bool
x `inside` ((ia,a),(ib,b)) = x > a && fun ia x a && x < b && fun ib x b
  where fun Incl = (==)
        fun Excl = (/=)
outside :: Ord a => a -> ((Inclusivity, a), (Inclusivity, a)) -> Bool
x `outside` i = not (x `inside` i)

-- | Two half-gaussians spread around mean
halfGaussians :: Double -> Double -> Double -> Double -> Double
halfGaussians sigmaBefore mean sigmaAfter x =
  let s | x <= mean = sigmaBefore
        | otherwise = sigmaAfter
  in gaussian mean s x

average :: Fractional a => [a] -> a
average l = tot/len
  where (tot, len) = foldl' (\(!t,!l) !x -> (t+x,l+1)) (0,0) l


type RandomStateM a = St.State StdGen a

randomM :: RandomStateM Double
randomM = do gen <- St.get
             let (x,gen') = random gen
             St.put gen'
             return x

splitM :: RandomStateM (t1 -> b) -> RandomStateM t1 -> RandomStateM b
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

