{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Data.Either (fromRight)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (singleton)
import Data.List.Split (chunksOf)
import Data.Text (lines, splitOn, strip, words)
import Data.Text.IO (readFile)
import Data.Text.Read (decimal)
import Prelude hiding (lines, readFile, words)

decimal' = fst . fromRight (error "yikes") . decimal

data Range = Range {start :: Int, len :: Int} deriving (Show)

toSeedRanges seeds =
  let [_, seedNumbers] = splitOn ": " seeds
   in words seedNumbers & map decimal' & chunksOf 2 & map (\[start, len] -> Range start len)

data Map = Map {dst :: Int, src :: Int, len' :: Int}

-- full subset
applyMap Map {dst, src, len'} Range {start, len}
  | src <= start && start + len <= src + len' =
      ([], [Range (start - src + dst) len])
-- part subset, bottom
applyMap Map {dst, src, len'} Range {start, len}
  | start < src && src < start + len && start + len <= src + len' =
      ([Range start (src - start)], [Range dst (start + len - src)])
-- part subset, top
applyMap Map {dst, src, len'} Range {start, len}
  | src <= start && start < src + len' && src + len' < start + len =
      ([Range (src + len') ((start + len) - (src + len'))], [Range (start - src + dst) (src + len' - start)])
-- part subset, both
applyMap Map {dst, src, len'} Range {start, len}
  | start < src && src + len' < start + len =
      ([Range start (src - start), Range (src + len') ((start + len) - (src + len'))], [Range dst len'])
-- disjoint
applyMap _ r = ([r], [])

toSection section =
  let _ : maps = lines section in map toMap maps
  where
    toMap map' = let [dst, src, len] = words map' & map decimal' in Map dst src len

applySection (map' : maps) rs =
  let (unmapped, mapped) = unzip $ map (applyMap map') rs
   in concat unmapped & applySection maps & (concat mapped ++)
applySection [] rs = rs

lowestLocation (seeds : sections) =
  let seedRanges = toSeedRanges seeds
   in let sections' = map toSection sections
       in foldl
            (\rs section -> concatMap (applySection section . singleton) rs)
            seedRanges
            sections'
            & map start
            & minimum

main = readFile "../../input" <&> strip <&> splitOn "\n\n" <&> lowestLocation >>= print
