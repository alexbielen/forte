module Transform
  ( fit,
    fitF,
    xToDx,
    dxToX,
    rotate,
    uniformQuantize,
    uniformQuantizeF,
    orderPitchfieldWithRow,
    FitMode (..),
    UniformMode (..),
  ) where

import PitchClass (midicentsToPitchClass)
data FitMode
  = Wrap
  | Clamp
  deriving (Show)

-- |
-- fit transforms a value to "fit" within a range according to a mode.
--
-- Wrap mode uses modular (aka "clock") arithmetic to "wrap" n into the range.
-- This is expressed in the following equation:
-- n' = n - floor((n - min) / (max - min )) *  (max - min)
--
-- Clamp mode takes any out-of-range values and replaces with the nearest bound.
fit :: Integral a => FitMode -> a -> a -> a -> a
fit mode lo hi n =
  if inRange
    then n
    else case mode of
      Wrap -> n - ((n - lo) `div` range) * range
      Clamp -> nearBound
  where
    inRange = lo <= n && n <= hi

    range = hi - lo

    -- the bound that is closest to n
    nearBound =
      if n > hi
        then hi
        else lo

-- |
-- fitF maps the fit function over functors.
fitF :: (Functor f, Integral a) => FitMode -> a -> a -> f a -> f a
fitF mode min max = fmap (fit mode min max)

-- |
-- Converts a list of points into a list of intervals.
--
-- You might want to use this to calculate the intervals in a 12-tone row or to
-- get the durations in a rhythmic series.
--
-- Example:
--
-- In: [5 1 4 2 3]
-- Out: [-4 3 -2 1]
xToDx :: (Num a) => [a] -> [a]
xToDx [] = []
xToDx [_] = []
xToDx l@(_ : xs) = zipWith diff l xs
  where
    diff a b = b - a

-- |
-- Creates a list of points from a start value and a list of intervals.
--
-- You might want to use this to calculate a melody from a starting note and a list of intervals.
dxToX :: (Num a) => a -> [a] -> [a]
dxToX start = foldl (\acc x -> acc ++ [last acc + x]) [start]

-- |
-- QuantizeMode
--
-- MidTread
-- MidRiser
data UniformMode
  = MidTread
  | MidRiser
  deriving (Show)

-- |
-- uniformQuantize transforms a Real number to the closest multiple of step. There are two modes:
--
-- MidTread uses the following algorithm:
--     step * floor((n / step) + 0.5)
--
-- MidRiser uses the following algorithm:
--     step * (floor(n / step)) + 0.5
uniformQuantize :: (RealFrac a) => UniformMode -> a -> a -> a
uniformQuantize mode step n = case mode of
  MidTread -> step * midTreadClassificationStage
  MidRiser -> step * (midRiserClassificationStage + 0.5)
  where
    midTreadClassificationStage = conv ((n / step) + 0.5)

    midRiserClassificationStage = conv (n / step)

    conv = fromIntegral . floor

-- |
-- uniformQuantizeF maps the uniformQuantize function over functors.
uniformQuantizeF :: (Functor f, RealFrac a) => UniformMode -> a -> f a -> f a
uniformQuantizeF mode step = fmap (uniformQuantize mode step)

-- |
-- rotate a list by n.
rotate :: Int -> [a] -> [a]
rotate = drop <> take

-- |
-- orderPitchfieldWithRow takes a row and a "pitchfield" and returns the pitchfield
-- in the order corresponding to the pitch classes from the row.
--
-- For example, the row [0, 4, 5, 2, 7, 9, 6, 8, 11, 10, 3, 1]
-- and pitchfield [6200,6300,6800,6900,7000,7600,8300,8900,9000,9100,9600,9700]
-- results in:
-- [9600, 7600, 8900, 6200, 9100, 6900, 9000, 6800, 8300, 7000, 6300, 9700]
orderPitchfieldWithRow :: Traversable t => [Int] -> t Int -> Maybe (t Int)
orderPitchfieldWithRow pitchfield = traverse (`lookup` ordered)
  where ordered = map (\a -> (midicentsToPitchClass a, a)) pitchfield
