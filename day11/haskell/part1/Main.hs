{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (transpose)

toImage input = lines input & map (map isGalaxy)
  where
    isGalaxy '#' = True
    isGalaxy '.' = False
    isGalaxy _ = undefined

expand image = expandVert image & transpose & expandVert & transpose
  where
    expandVert [] = []
    expandVert (row : rest) | all not row = row : row : expandVert rest
    expandVert (row : rest) = row : expandVert rest

shortestPathSum image = combinations galaxies & map manhattan & sum
  where
    galaxies = concat [[(y, x) | (x, isGalaxy) <- zip [(0 :: Int) ..] row, isGalaxy] | (y, row) <- zip [0 ..] image]
    manhattan ((a, b), (c, d)) = abs (a - c) + abs (b - d)
    combinations [] = []
    combinations (x : xs) = [(x, y) | y <- xs] ++ combinations xs

main = readFile "../../input" <&> toImage <&> expand <&> shortestPathSum >>= print
