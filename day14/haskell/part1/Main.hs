{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate, transpose)
import Data.List.Split (splitOn)

data Cell = Round | Cube | Empty deriving (Eq)

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

load = map load' >>> sum
  where
    load' = zip [(1 :: Int) ..] >>> filter (snd >>> (== Round)) >>> map fst >>> sum

main = readFile "../../input" <&> toPlatform <&> tilt <&> load >>= print
