{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Control.Arrow ((>>>))
import Data.Array (assocs, bounds, listArray, (!))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (fromList, member)

data Direction = V | H | NE | NW | SW | SE deriving (Eq, Enum, Bounded)

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

data Tile' = Ground' | Pipe' Direction deriving (Eq)

toReduced sketch = assocs sketch & map reduce & listArray (bounds sketch)
  where
    [(startYx :: (Int, Int), Start)] = assocs sketch & filter (snd >>> (== Start))
    loop = findLoop sketch [startYx]
    loopSet = fromList loop

    findSecondLast [] = error "no second last"
    findSecondLast [x, _] = x
    findSecondLast (_ : xs) = findSecondLast xs
    secondLast = findSecondLast loop

    _ : second : _ = loop

    startNeighbourSet = fromList [second, secondLast]
    matches d = startNeighbourSet == fromList (neighbours d startYx)

    reduce (_, Start) = Pipe' $ head $ filter matches $ enumFrom V
    reduce (yx, Pipe d) | yx `member` loopSet = Pipe' d
    reduce _ = Ground'

data State = Out | In

swap Out = In
swap In = Out

enclosedGround sketch = assocs reduced & map snd & chunksOf width & map (enclosedGround' Out) & sum
  where
    ((_, 1), (_, width)) = bounds sketch
    reduced = toReduced sketch

    enclosedGround' Out (Ground' : rest) = enclosedGround' Out rest
    enclosedGround' In (Ground' : rest) = (1 :: Int) + enclosedGround' In rest
    enclosedGround' s (Pipe' V : rest) = enclosedGround' (swap s) rest
    enclosedGround' s (Pipe' NE : rest) = do
      let (Pipe' c : rest') = dropWhile (== Pipe' H) rest
      case c of
        NW -> enclosedGround' s rest'
        SW -> enclosedGround' (swap s) rest'
        _ -> undefined
    enclosedGround' s (Pipe' SE : rest) = do
      let (Pipe' c : rest') = dropWhile (== Pipe' H) rest
      case c of
        NW -> enclosedGround' (swap s) rest'
        SW -> enclosedGround' s rest'
        _ -> undefined
    enclosedGround' Out [] = 0
    enclosedGround' _ _ = undefined

main = readFile "../../input" <&> toSketch <&> enclosedGround >>= print
