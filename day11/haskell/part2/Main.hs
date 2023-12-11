{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Control.Arrow ((>>>))
import Data.Array (listArray, (!))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (transpose)

expansionFactor = 1_000_000 - 1

toImage input = lines input & map (map isGalaxy)
  where
    isGalaxy '#' = True
    isGalaxy '.' = False

expand' image = [(emptyRowsBefore ! y * expansionFactor + y, emptyColsBefore ! x * expansionFactor + x) | (y, x) <- galaxies]
  where
    galaxies = concat [[(y, x) | (x, isGalaxy) <- zip [(1 :: Int) ..] row, isGalaxy] | (y, row) <- zip [1 ..] image]
    emptyBefore grid = map (all not >>> fromEnum) grid & scanl (+) 0 & init & listArray (1, length grid)
    emptyRowsBefore = emptyBefore image
    emptyColsBefore = emptyBefore $ transpose image

shortestPathSum galaxies = combinations galaxies & map manhattan & sum
  where
    manhattan ((a, b), (c, d)) = abs (a - c) + abs (b - d)
    combinations [] = []
    combinations (x : xs) = [(x, y) | y <- xs] ++ combinations xs

main = readFile "../../input" <&> toImage <&> expand' <&> shortestPathSum >>= print
