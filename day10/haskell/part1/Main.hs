{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Control.Arrow ((>>>))
import Data.Array (assocs, bounds, listArray, (!))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, listToMaybe)

data Direction = V | H | NE | NW | SW | SE deriving (Eq)

toDirection '|' = V
toDirection '-' = H
toDirection 'L' = NE
toDirection 'J' = NW
toDirection '7' = SW
toDirection 'F' = SE
toDirection c = error $ "invalid direction" ++ show c

neighbours V (y, x) = [(y - 1, x), (y + 1, x)]
neighbours H (y, x) = [(y, x - 1), (y, x + 1)]
neighbours NE (y, x) = [(y - 1, x), (y, x + 1)]
neighbours NW (y, x) = [(y - 1, x), (y, x - 1)]
neighbours SW (y, x) = [(y + 1, x), (y, x - 1)]
neighbours SE (y, x) = [(y + 1, x), (y, x + 1)]

data Tile = Start | Ground | Pipe Direction deriving (Eq)

neighbours' Start (y, x) = [(y, x - 1), (y, x + 1), (y - 1, x), (y + 1, x)]
neighbours' Ground _ = []
neighbours' (Pipe d) (x, y) = neighbours d (x, y)

toTile 'S' = Start
toTile '.' = Ground
toTile c = Pipe $ toDirection c

toSketch input = do
  let lines' = lines input
  listArray ((1, 1), (length lines', length $ head lines')) $ concatMap (map toTile) lines'

findLoop sketch (yx : cb : cx) | sketch ! yx == Start = yx : cb : cx
findLoop sketch (yx : cx) =
  case (filter isNext currentNeighbours, currentTile) of
    (next : _, Start) -> findLoop sketch $ next : yx : cx
    ([next], _) -> findLoop sketch $ next : yx : cx
    _ -> undefined
  where
    currentTile = sketch ! yx
    currentNeighbours = neighbours' currentTile yx
    prev = fromMaybe (0, 0) (listToMaybe cx)
    notPrev = (/= prev)
    ((minY, minX), (maxY, maxX)) = bounds sketch
    inBounds (y, x) = minY <= y && minX <= x && y <= maxY && x <= maxX
    isNext yx' = inBounds yx' && notPrev yx' && elem yx (neighbours' (sketch ! yx') yx')
findLoop _ [] = undefined

farthestPoint sketch = (length (findLoop sketch [startYx]) - 1) `div` 2
  where
    [(startYx :: (Int, Int), Start)] = assocs sketch & filter (snd >>> (== Start))

main = readFile "../../input" <&> toSketch <&> farthestPoint >>= print
