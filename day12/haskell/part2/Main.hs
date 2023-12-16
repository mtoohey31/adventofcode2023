{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Control.Arrow ((>>>))
import Control.Monad.Memo (memo, startEvalMemo)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.List.Split (splitOn)

data Cond = Operational | Damaged | Unknown deriving (Eq, Ord)

toCond '.' = Operational
toCond '#' = Damaged
toCond '?' = Unknown

data Record = Record {springs :: [Cond], groups :: [Int]}

toRecord line = do
  let [springs, groups] = words line
  let springs' = map toCond springs & (repeat >>> take 5) & intercalate [Unknown]
  let groups' = splitOn "," groups & map read & (repeat >>> take 5 >>> concat)
  Record springs' groups'

arrangements Record {springs, groups} = startEvalMemo (arrangements' (springs, groups, Nothing))
  where
    -- Progress with no ongoing group when we can't start a new group:
    arrangements' (Operational : springsRest, groups, Nothing) = arrangements' (springsRest, groups, Nothing)
    -- Progress with no remaining groups:
    arrangements' (Unknown : springsRest, [], Nothing) = arrangements' (springsRest, [], Nothing)
    -- Need to start group:
    arrangements' (Damaged : springsRest, group : groups, Nothing) = arrangements' (springsRest, groups, Just (group - 1))
    -- Can choose whether to start next group:
    arrangements' (Unknown : springsRest, group : groups, Nothing) = do
      start <- memo arrangements' (springsRest, groups, Just (group - 1))
      noStart <- memo arrangements' (springsRest, group : groups, Nothing)
      return $ start + noStart
    -- Need to leave gap before next group:
    arrangements' (spring : springsRest, groups, Just 0) = case spring of
      Operational -> arrangements' (springsRest, groups, Nothing)
      Damaged -> return (0 :: Int) -- group we just applied wasn't long enough
      Unknown -> arrangements' (springsRest, groups, Nothing)
    -- Need to continue current group:
    arrangements' (Damaged : springsRest, groups, Just n) = arrangements' (springsRest, groups, Just (n - 1))
    arrangements' (Unknown : springsRest, groups, Just n) = arrangements' (springsRest, groups, Just (n - 1))
    -- Can't continue current group:
    arrangements' (Operational : _, _, Just _) = return 0
    -- Can't start new group:
    arrangements' (Damaged : _, [], Nothing) = return 0
    -- Finished at the end:
    arrangements' ([], [], Just 0) = return 1
    arrangements' ([], [], Nothing) = return 1
    -- Groups left at the end:
    arrangements' ([], _ : _, _) = return 0
    arrangements' ([], [], Just _) = return 0

main = (readFile "../../input" <&> lines <&> map (toRecord >>> arrangements)) >>= print . sum
