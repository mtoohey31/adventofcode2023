{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Split (splitOn)

data Cond = Operational | Damaged | Unknown

toCond '.' = Operational
toCond '#' = Damaged
toCond '?' = Unknown

data Record = Record {springs :: [Cond], groups :: [Int]}

toRecord line = do
  let [springs, groups] = words line
  let springs' = map toCond springs
  let groups' = splitOn "," groups & map read
  Record springs' groups'

arrangements Record {springs, groups} = arrangements' springs groups Nothing
  where
    -- Progress with no ongoing group when we can't start a new group:
    arrangements' (Operational : springsRest) groups Nothing = arrangements' springsRest groups Nothing
    -- Progress with no remaining groups:
    arrangements' (Unknown : springsRest) [] Nothing = arrangements' springsRest [] Nothing
    -- Need to start group:
    arrangements' (Damaged : springsRest) (group : groups) Nothing = arrangements' springsRest groups (Just (group - 1))
    -- Can choose whether to start next group:
    arrangements' (Unknown : springsRest) (group : groups) Nothing = do
      let start = arrangements' springsRest groups (Just (group - 1))
      let noStart = arrangements' springsRest (group : groups) Nothing
      start + noStart
    -- Need to leave gap before next group:
    arrangements' (spring : springsRest) groups (Just 0) = case spring of
      Operational -> arrangements' springsRest groups Nothing
      Damaged -> (0 :: Int) -- group we just applied wasn't long enough
      Unknown -> arrangements' springsRest groups Nothing
    -- Need to continue current group:
    arrangements' (Damaged : springsRest) groups (Just n) = arrangements' springsRest groups (Just (n - 1))
    arrangements' (Unknown : springsRest) groups (Just n) = arrangements' springsRest groups (Just (n - 1))
    -- Can't continue current group:
    arrangements' (Operational : _) _ (Just _) = 0
    -- Can't start new group:
    arrangements' (Damaged : _) [] Nothing = 0
    -- Finished at the end:
    arrangements' [] [] (Just 0) = 1
    arrangements' [] [] Nothing = 1
    -- Groups left at the end:
    arrangements' [] (_ : _) _ = 0
    arrangements' [] [] (Just _) = 0

main = readFile "../../input" <&> lines <&> map (toRecord >>> arrangements) <&> sum >>= print
