{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Map (fromList, (!))
import Prelude hiding (Left, Right)

data Instruction = Left | Right

toInstruction c = case c of
  'L' -> Left
  'R' -> Right

start = "AAA"

end = "ZZZ"

toNode s =
  let [from, to] = splitOn " = " s
   in let [left, right] = to & tail & init & splitOn ", "
       in (from, (left, right))

steps' n _ _ | n == end = 0 :: Int
steps' n net (i : ir) = case (net ! n, i) of
  ((nn, _), Left) -> 1 + steps' nn net ir
  ((_, nn), Right) -> 1 + steps' nn net ir

steps input =
  let [instructions, nodes] = splitOn "\n\n" input
   in let instructions' = map toInstruction instructions & cycle
       in let net = nodes & lines & map toNode & fromList
           in steps' start net instructions'

main = readFile "../../input" <&> steps >>= print
