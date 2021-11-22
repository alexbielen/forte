module PitchClass
  ( orderedInterval,
    unorderedInterval,
    invert,
    transpose,
    transposeF,
    midicentsToPitchClass
  )
where

import Data.List (group, sort)

_mod12 :: Integral a => a -> a
_mod12 = flip mod 12

-- |
-- orderedInterval returns the number of ascending semi-tones
-- between two pitch classes (0-11).
--
-- For example, C to E is 4 semitones, but E to C is 8 semitones (or the "inversion" of 4 semitones.)
orderedInterval :: Integral a => a -> a -> a
orderedInterval n1 n2 =
  if n1 <= n2
    then distance
    else invert distance
  where
    distance = _mod12 . abs $ (n1 - n2)

unorderedInterval :: Integral a => a -> a -> a
unorderedInterval n1 n2 = min a b
  where
    a = _mod12 (n1 - n2)

    b = _mod12 (n2 - n1)

-- |
-- invert returns the pitch class inverted around C (0).
--
-- For example, E (4) inverts to Ab (8).
invert :: Integral a => a -> a
invert = (-) 12

-- |
-- transpose pitch n by some interval.
transpose :: Integral a => a -> a -> a
transpose n by = _mod12 (n + by)


-- |
-- transpose a function by some interval
transposeF :: (Functor f, Integral b) => b -> f b -> f b
transposeF n = fmap (transpose n)


-- |
-- Convert midicents to pitchclasses.
midicentsToPitchClass :: Int -> Int
midicentsToPitchClass n = _mod12 (n `div` 100)
