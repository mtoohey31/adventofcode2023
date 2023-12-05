{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Data.Either (fromRight)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (lines, splitOn, strip, words)
import Data.Text.IO (readFile)
import Data.Text.Read (decimal)
import Prelude hiding (lines, readFile, words)

decimal' = fst . fromRight (error "yikes") . decimal

toSeed seeds =
  let [_, seedNumbers] = splitOn ": " seeds
   in words seedNumbers & map decimal'

data Map = Map {dst :: Int, src :: Int, len :: Int}

applyMap Map {dst, src, len} v | src <= v && v < src + len = Just $ v - src + dst
applyMap _ _ = Nothing

toSection section =
  let _ : maps = lines section in map toMap maps
  where
    toMap map' = let [dst, src, len] = words map' & map decimal' in Map dst src len

applySection section v = mapMaybe (`applyMap` v) section & listToMaybe & fromMaybe v

lowestLocation (seeds : sections) =
  let seeds' = toSeed seeds
   in let sections' = map toSection sections
       in map (flip (foldl (flip applySection)) sections') seeds' & minimum

main = readFile "../../input" <&> strip <&> splitOn "\n\n" <&> lowestLocation >>= print
