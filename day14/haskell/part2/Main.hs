{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate, transpose)
import Data.List.Split (splitOn)
import Data.Map (insert, lookup)
import Data.Map qualified
import Prelude hiding (lookup)

data Cell = Round | Cube | Empty deriving (Eq, Ord)

toPlatform = lines >>> map (map toCell) >>> transpose >>> map reverse
  where
    toCell 'O' = Round
    toCell '#' = Cube
    toCell '.' = Empty
    toCell _ = undefined

tilt = map tilt'
  where
    tilt' col = splitOn [Cube] col & map tilt'' & intercalate [Cube]
    tilt'' section = replicate (length' - rounds) Empty ++ replicate rounds Round
      where
        length' = length section
        rounds = length $ filter (== Round) section

spin = iterate (tilt >>> rotate) >>> (!! 4)
  where
    rotate = transpose >>> map reverse

findRepeat = findRepeat' Data.Map.empty (0 :: Int)
  where
    findRepeat' seen i platform =
      case lookup platform seen of
        Just n -> (n, i, platform)
        Nothing -> findRepeat' (insert platform i seen) (i + 1) (spin platform)

iterateSpin platform = do
  let (first, second, platform') = findRepeat platform
  iterate spin platform' !! ((1000000000 - first) `rem` (second - first))

load = map load' >>> sum
  where
    load' = zip [(1 :: Int) ..] >>> filter (snd >>> (== Round)) >>> map fst >>> sum

main = readFile "../../input" <&> toPlatform <&> iterateSpin <&> load >>= print
