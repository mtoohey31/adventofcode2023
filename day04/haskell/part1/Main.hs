{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)

data Card = Card {winningNumbers :: Set Int, numbers :: [Int]}

toCard line =
  let [_, allNumbers] = splitOn ": " line
   in let [winningNumbers, numbers] = splitOn " | " allNumbers
       in let winningNumbers' = winningNumbers & words & map read & fromList
           in let numbers' = numbers & words & map read
               in Card winningNumbers' numbers'

score Card {winningNumbers, numbers} =
  if matches > 0
    then (2 :: Int) ^ (matches - 1)
    else 0
  where
    matches = filter (`member` winningNumbers) numbers & length

main = readFile "../../input" <&> lines <&> map toCard <&> map score <&> sum >>= print
