{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Map (fromList, (!))
import Prelude hiding (Left, Right, repeat)

data Instruction = Left | Right

toInstruction c = case c of
  'L' -> Left
  'R' -> Right

startPrefix = 'A'

endSuffix = 'Z'

toNode s = do
  let [from, to] = splitOn " = " s
  let [left, right] = to & tail & init & splitOn ", "
  (from, (left, right))

step n net (i : ir) = case (net ! n, i) of
  ((n', _), Left) -> (n', ir)
  ((_, n'), Right) -> (n', ir)

data Pattern = Pattern {phase :: Int, period :: Int}

toPattern net is n = do
  let (first, n', is') = stepsToEnd n net is
  let (n'', is'') = step n' net is'
  let (second, _, _) = stepsToEnd n'' net is''
  Pattern (second + 1 - first) (second + 1)

stepsToEnd n _ is | last n == endSuffix = (0 :: Int, n, is)
stepsToEnd n net is = do
  let (n', is') = step n net is
  let (i, n'', is'') = stepsToEnd n' net is'
  (i + 1, n'', is'')

-- https://math.stackexchange.com/a/3864593
solvePatterns patterns =
  let Pattern {phase, period} = foldl1 combine patterns in period - phase
  where
    combine Pattern {phase = ph1, period = p1} Pattern {phase = ph2, period = p2} = do
      let (gcd, s, _) = extendedGcd p1 p2
      let phDiff = ph1 - ph2
      let (phDiffMult, phDiffRem) = divMod phDiff gcd
      if phDiffRem /= 0
        then error "no sync"
        else do
          let combinedP = p1 `div` gcd * p2
          let combinedPh = (ph1 - s * phDiffMult * p1) `rem` combinedP
          Pattern combinedPh combinedP
    extendedGcd a b = extendedGcd' a b a b 1 0 0 1
    extendedGcd' _ _ oldR r oldS _ oldT _ | r == 0 = (oldR, oldS, oldT)
    extendedGcd' a b oldR r oldS s oldT t = do
      let (quotient, remainder) = divMod oldR r
      let newR = remainder
      let newS = oldS - quotient * s
      let newT = oldT - quotient * t
      extendedGcd' a b r newR s newS t newT

stepsFromInput input = do
  let [instructions, nodes] = splitOn "\n\n" input
  let instructions' = map toInstruction instructions & cycle
  let nodes' = nodes & lines & map toNode
  let net = fromList nodes'
  let startNodes = filter ((== startPrefix) . last) $ map fst nodes'
  map (toPattern net instructions') startNodes & solvePatterns

main = readFile "../../input" <&> stepsFromInput >>= print
