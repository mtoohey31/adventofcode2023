{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Data.Array (bounds, listArray, (!))
import Data.Char (isDigit, ord)
import Data.Function ((&))
import Data.Functor ((<&>))

data Cell = Digit Int | Symbol | Empty deriving (Eq)

toCell c = case c of
  c | isDigit c -> Digit (ord c - ord '0')
  '.' -> Empty
  _ -> Symbol

toSchema lines =
  concatMap (map toCell) lines & listArray ((1, 1), (length lines, head lines & length))

hasAdjacentSymbol schema (row, (colStart, colEnd)) =
  filter
    inBounds
    ( (row, colStart - 1)
        : (row, colEnd + 1)
        : [(row - 1, col) | col <- [colStart - 1 .. colEnd + 1]]
        ++ [(row + 1, col) | col <- [colStart - 1 .. colEnd + 1]]
    )
    & any
      ((== Symbol) . (schema !))
  where
    ((rowMin, colMin), (rowMax, colMax)) = bounds schema
    inBounds (row, col) = rowMin <= row && row <= rowMax && colMin <= col && col <= colMax

rowPartNumbersSum schema row = rowPartNumbersSum' colMin
  where
    ((_, colMin), (_, colMax)) = bounds schema
    rowPartNumbersSum' col | col > colMax = 0
    rowPartNumbersSum' col = case schema ! (row, col) of
      Digit _ ->
        let (number, colEnd) = readNumber col
         in if hasAdjacentSymbol schema (row, (col, colEnd))
              then number + rowPartNumbersSum' (colEnd + 1)
              else rowPartNumbersSum' (col + 1)
      _ -> rowPartNumbersSum' (col + 1)
    readNumber col | col > colMax = (0, colMax)
    readNumber col = case schema ! (row, col) of
      Digit i ->
        let (numberRest, colMax) = readNumber (col + 1)
         in (i * (10 ^ (colMax - col)) + numberRest, colMax)
      _ -> (0, col - 1)

partNumbersSum schema = map (rowPartNumbersSum schema) [rowMin .. rowMax] & sum
  where
    ((rowMin, _), (rowMax, _)) = bounds schema

main = readFile "../../input" <&> lines <&> toSchema <&> partNumbersSum >>= print
