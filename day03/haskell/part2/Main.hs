{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}
{-# HLINT ignore "Functor law" #-}

import Data.Array (bounds, indices, listArray, (!))
import Data.Char (isDigit, ord)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (nub)

data Cell = Digit Int | Star | Other deriving (Eq)

toCell c = case c of
  c | isDigit c -> Digit (ord c - ord '0')
  '*' -> Star
  _ -> Other

toSchema lines =
  concatMap (map toCell) lines & listArray ((1, 1), (length lines, head lines & length))

starIndices schema = indices schema & filter ((== Star) . (schema !))

gearRatio schema (row, col) =
  let gears =
        filter
          inBounds
          ( (row, col - 1)
              : (row, col + 1)
              : [(row - 1, col) | col <- [col - 1 .. col + 1]]
              ++ [(row + 1, col) | col <- [col - 1 .. col + 1]]
          )
          & filter (isDigit . (schema !))
          & map readNumber
          & nub
   in if length gears < 2 then 0 else map fst gears & product
  where
    ((rowMin, colMin), (rowMax, colMax)) = bounds schema
    inBounds (row, col) = rowMin <= row && row <= rowMax && colMin <= col && col <= colMax
    readNumber (row, col) | col < colMin = readNumber' (row, colMin)
    readNumber (row, col) = case schema ! (row, col) of
      Digit _ -> readNumber (row, col - 1)
      _ -> readNumber' (row, col + 1)
    readNumber' (_, col) | col > colMax = (0, colMax)
    readNumber' (row, col) = case schema ! (row, col) of
      Digit i ->
        let (numberRest, colMax) = readNumber' (row, col + 1)
         in (i * (10 ^ (colMax - col)) + numberRest, colMax)
      _ -> (0, col - 1)
    isDigit c = case c of
      Digit _ -> True
      _ -> False

gearRatiosSum schema = starIndices schema & map (gearRatio schema) & sum

main = readFile "../../input" <&> lines <&> toSchema <&> gearRatiosSum >>= print
