{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Control.Arrow ((>>>))
import Data.Array (bounds, listArray, (!))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.HashSet (insert, member, toList)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable (hashWithSalt))
import Data.List (singleton)
import Relude.Nub (hashNub)
import Prelude hiding (Left, Right)

data MirrorDir = Forward | Backward

data SplitterDir = Vert | Horiz

data Tile = Empty | Mirror MirrorDir | Splitter SplitterDir

toGrid input = do
  let lines' = lines input
  concatMap (map toTile) lines'
    & listArray ((1, 1), (length lines', length $ head lines'))
  where
    toTile '.' = Empty
    toTile '/' = Mirror Forward
    toTile '\\' = Mirror Backward
    toTile '|' = Splitter Vert
    toTile '-' = Splitter Horiz
    toTile _ = undefined

data Dir = Up | Down | Left | Right deriving (Eq, Ord)

instance Hashable Dir where
  hashWithSalt salt Up = salt
  hashWithSalt salt Down = salt + 1
  hashWithSalt salt Left = salt + 2
  hashWithSalt salt Right = salt + 3

next Up (Mirror Forward) = [Right]
next Down (Mirror Forward) = [Left]
next Left (Mirror Forward) = [Down]
next Right (Mirror Forward) = [Up]
next Up (Mirror Backward) = [Left]
next Down (Mirror Backward) = [Right]
next Left (Mirror Backward) = [Up]
next Right (Mirror Backward) = [Down]
next Left (Splitter Vert) = [Up, Down]
next Right (Splitter Vert) = [Up, Down]
next Up (Splitter Horiz) = [Left, Right]
next Down (Splitter Horiz) = [Left, Right]
next Up _ = [Up]
next Down _ = [Down]
next Left _ = [Left]
next Right _ = [Right]

move (y, x) Up = (y - 1, x)
move (y, x) Down = (y + 1, x)
move (y, x) Left = (y, x - 1)
move (y, x) Right = (y, x + 1)

energySets grid = map (singleton >>> traceRays HashSet.empty) allStarts
  where
    traceRays energySet' [] = energySet'
    traceRays energySet' ((yx, d) : rx) =
      if (yx, d) `member` energySet'
        then traceRays energySet' rx
        else traceRays (insert (yx, d) energySet') $ newRays ++ rx
      where
        newRays = next d (grid ! yx) & map (\d' -> (move yx d', d')) & filter (fst >>> inBounds)
    ((yMin, xMin), (yMax, xMax)) = bounds grid
    inBounds (y, x) = yMin <= y && y <= yMax && xMin <= x && x <= xMax
    topStarts = map ((yMin,) >>> (,Down)) [xMin .. xMax]
    botStarts = map ((yMax,) >>> (,Up)) [xMin .. xMax]
    leftStarts = map ((,xMin) >>> (,Right)) [yMin .. yMax]
    rightStarts = map ((,xMax) >>> (,Left)) [yMin .. yMax]
    allStarts = concat [topStarts, botStarts, leftStarts, rightStarts]

energiedTiles = energySets >>> map (toList >>> map fst >>> hashNub >>> length) >>> maximum

main = readFile "../../input" <&> toGrid <&> energiedTiles >>= print
