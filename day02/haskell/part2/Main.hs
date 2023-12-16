{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <$>" #-}

import Data.List.Split (splitOn)
import Prelude hiding (id)

data Counts = Counts {red :: Int, green :: Int, blue :: Int}

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

toMaxCounts s =
  let [_, handfulls] = splitOn ": " s
   in toMaxCounts' handfulls
  where
    toMaxCounts' = toMaxCounts'' . map toCounts . splitOn "; "
    toMaxCounts'' counts =
      Counts
        (foldl max 0 $ map red counts)
        (foldl max 0 $ map green counts)
        (foldl max 0 $ map blue counts)

power Counts {red, green, blue} = red * green * blue

main =
  sum <$> map power <$> map toMaxCounts <$> lines <$> readFile "../../input"
    >>= print
