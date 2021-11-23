module Main where

import Data.Maybe (fromMaybe)
import Data.List (zip4)
import PitchClass (transpose, transposeF)
import Transform (dxToX, orderPitchfieldWithRow, rotate)

main :: IO ()
main = do
  let luluRow = [0, 4, 5, 2, 7, 9, 6, 8, 11, 10, 3, 1]
      progression =
        -- 12-tone chords with symmetrical intervals; each type has unique intervals allowed.
        [ [100, 500, 100, 100, 600, 700, 600, 100, 100, 500, 100], -- dissonant chord 1
          [300, 500, 200, 700, 100, 700, 100, 700, 200, 500, 300], -- diatonic chord 1
          [500, 400, 700, 400, 500, 500, 500, 400, 700, 400, 500], -- quartal chord 1
          [500, 200, 300, 400, 600, 700, 600, 400, 300, 200, 500], -- diatonic chord 2
          [600, 100, 600, 100, 600, 100, 600, 100, 600, 100, 600], -- dissonant chord 2
          [500, 200, 700, 700, 600, 700, 600, 700, 700, 200, 500], -- diatonic chord 3
          [400, 300, 400, 400, 500, 500, 500, 400, 400, 300, 400], -- quartal chord 2
          [700, 200, 500, 200, 600, 700, 600, 200, 500, 200, 700] -- diatonic chord 4
        ]
      luluProg = pianoPitchMaterial luluRow progression
      system1 = rotate 6 $ concat $ luluProg 2200 -- rotate 6
      system2 = concat $ luluProg 3000
      system3 = rotate 3 $ concat $ luluProg 3400 -- rotate 3
      system4 = rotate 9 $ concat $ luluProg 3600 -- rorate 9
      result = concatMap quadToList (zip4 system1 system2 system3 system4)

  print result

pianoPitchMaterial :: [Int] -> [[Int]] -> Int -> [[Int]]
pianoPitchMaterial row progression startNote = zipWith orderedField transposedLuluRows (cycle pitchFields)
  where
    luluBass = map (\x -> (x * 100) + startNote) row
    pitchFields = zipWith dxToX luluBass progression
    transposedLuluRows = map (`transposeF` row) row

orderedField :: [Int] -> [Int] -> [Int]
orderedField row pitchField = fromMaybe [] $ orderPitchfieldWithRow pitchField row

tripToList :: (a, a, a) -> [a] -- :TODO hacky, maybe do this with ZipList? Or a lens or something. 
tripToList (a, b, c) = [a, b, c]

quadToList :: (a, a, a, a) -> [a]
quadToList (a, b, c, d) = [a, b, c, d]
