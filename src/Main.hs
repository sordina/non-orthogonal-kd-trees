{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Raster.Field
import System.Random
import Data.Semigroup
import GHC.Base (NonEmpty(..))

import qualified Linear.V2     as LV2
import qualified Linear.Metric as Metric

-- Data and instances

data Collection a = I ! a                                         -- Either, a point
                  | T ! (Cut a) ! (Collection a) ! (Collection a) -- Or, a cut and a pair of collections
                  deriving (Show)                                 -- The cut is represented by an origin, and a target.
                                                                  -- Positivity is calculated by dot product.
                                                                  -- The first target is arbitrary and will be the first point.
                                                                  -- The pair of collections will be negative, then positive

type V2 = LV2.V2 Float

type Cut a = (a, a) -- Origin, Target

instance (Metric.Metric f, Ord a, Num a, Fractional (f a)) => Semigroup (Collection (f a)) where
  I p <> T f a b
    | positive p f = T f a (I p <> b)
    | otherwise    = T f (I p <> a) b
  I p     <> I q   = T (mid p q, q) (I p) (I q)
  T _ a b <> y     = a <> (b <> y)

-- Definitions

positive :: (Ord a, Metric.Metric f, Num a, Num (f a)) => f a -> (f a, f a) -> Bool
positive p (o,t) = Metric.dot a b > 0
  where
  a = t - o
  b = p - o

mid :: Fractional a => a -> a -> a
mid a b = (a + b) / 2

index :: (Metric.Metric f, Ord a, Num a, Fractional (f a)) => NonEmpty (f a) -> Collection (f a)
index = sconcat . fmap I

closest :: (Ord a, Metric.Metric f, Num a, Num (f a)) => f a -> Collection (f a) -> f a
closest _ (I p) = p
closest p (T f c1 c2)
  | positive p f = closest p c2
  | otherwise    = closest p c1

-- Main and Top level Helpers

main :: IO ()
main = do
  g <- randomRs (-1,1) <$> newStdGen
  h <- randomRs (-1,1) <$> newStdGen
  let ps' = zipWith LV2.V2 g h
  let ps  = head ps' :| take 200 (tail ps')
  pp 0 $ index ps
  animateField FullScreen (1,1) (\t p -> mkColor t (closest (uncurry LV2.V2 (rejig t p)) (index (mvPoints t ps)))) -- Show Nearest Point

rejig :: Float -> Point -> Point
rejig t (x,y) = (sin (t*3+x*2) / 3 + x, sin (t*3+y*4) / 5 + y)

pp :: Show a => Int -> Collection a -> IO ()
pp n (I p) = putStr (replicate n ' ') >> print p
pp n (T _ a b) = do
  putStr (replicate n ' ') >> putStrLn "TL"
  pp (n + 2) a
  putStr (replicate n ' ') >> putStrLn "TR"
  pp (n + 2) b

mvPoints :: (Functor f, Floating a) => a -> f (LV2.V2 a) -> f (LV2.V2 a)
mvPoints t ps = fmap (\(LV2.V2 x y) -> LV2.V2 ((sin (t + y) / 20) + x) ((sin (2 * t + x) / 20 + y))) ps

mkColor :: Float -> LV2.V2 Float -> Color
mkColor t (LV2.V2 x y) = rgb (foo x) (foo y) (foo (sin t)) where foo i = succ i / 2
