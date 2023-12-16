{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Redundant map" #-}

import Data.List.Split (splitOn)
import Prelude hiding (id)

data Counts = Counts {red :: Int, green :: Int, blue :: Int}

data Game = Game {id :: Int, maxCounts :: Counts}

toCounts = toCounts' . splitOn ", "
  where
    toCounts' (c : cs) =
      let [countString, colour] = splitOn " " c
       in let count = read countString
           in let Counts {red, green, blue} = toCounts' cs
               in case colour of
                    "red" -> Counts count green blue
                    "green" -> Counts red count blue
                    "blue" -> Counts red green count
                    _ -> undefined
    toCounts' [] = Counts 0 0 0

toGame s =
  let [prefix, handfulls] = splitOn ": " s
   in let [_, idString] = splitOn " " prefix
       in let id = read idString in Game id $ toMaxCounts handfulls
  where
    toMaxCounts = toMaxCounts' . map toCounts . splitOn "; "
    toMaxCounts' counts =
      Counts
        (foldl max 0 $ map red counts)
        (foldl max 0 $ map green counts)
        (foldl max 0 $ map blue counts)

possible Game {maxCounts} = (red maxCounts <= 12) && (green maxCounts <= 13) && (blue maxCounts <= 14)

main =
  sum <$> map id <$> filter possible <$> map toGame <$> lines <$> readFile "../../input"
    >>= print
