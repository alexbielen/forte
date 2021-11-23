module Main where

import Data.Maybe (fromMaybe)
import Transform (dxToX, orderPitchfieldWithRow)
import PitchClass (transpose, transposeF)


-- 2200, 3000, 3400


main :: IO ()
main = do
  let luluRow = [0, 4, 5, 2, 7, 9, 6, 8, 11, 10, 3, 1]
      progression = -- 12-tone chords with symmetrical intervals; each type has unique intervals allowed.
        [ [100, 500, 100, 100, 600, 700, 600, 100, 100, 500, 100], -- dissonant chord 1
          [300, 500, 200, 700, 100, 700, 100, 700, 200, 500, 300], -- diatonic chord 1
          [500, 400, 700, 400, 500, 500, 500, 400, 700, 400, 500], -- quartal chord 1
          [500, 200, 300, 400, 600, 700, 600, 400, 300, 200, 500], -- diatonic chord 2
          [600, 100, 600, 100, 600, 100, 600, 100, 600, 100, 600], -- dissonant chord 2
          [500, 200, 700, 700, 600, 700, 600, 700, 700, 200, 500], -- diatonic chord 3
          [400, 300, 400, 400, 500, 500, 500, 400, 400, 300, 400], -- quartal chord 2
          [700, 200, 500, 200, 600, 700, 600, 200, 500, 200, 700]  -- diatonic chord 4
        ]

      -- for each chord, create a pitchfield using the Lulu row as the bottom note of each
      luluBass = map (\x -> (x * 100) + 3400) luluRow
      pitchFields = zipWith dxToX luluBass progression

      -- transpose the Lulu row by itself
      transposedLuluRows = map (`transposeF` luluRow) luluRow

      -- order the pitchfields according to the transposed Lulu row
      orderedFields = concat $ zipWith orderedField transposedLuluRows (cycle pitchFields)


  print orderedFields


orderedField :: [Int] -> [Int] -> [Int]
orderedField row pitchField = fromMaybe [] $ orderPitchfieldWithRow pitchField row
