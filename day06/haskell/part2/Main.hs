{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Split (splitOn)

winningWays (time :: Int) distance = upper - lower + 1 `max` 0 :: Int
  where
    t = fromIntegral time :: Double
    -- Quadratic formula.
    common = sqrt $ fromIntegral $ time ^ (2 :: Int) - 4 * distance
    lower = floor $ (t - common) / 2 + 1
    upper = ceiling $ (t + common) / 2 - 1

winningWaysProduct inputText =
  let [["Time", times], ["Distance", distances]] = lines inputText & map (splitOn ": ")
   in let time = times & words & concat & read
       in let distance = distances & words & concat & read
           in winningWays time distance

main = readFile "../../input" <&> winningWaysProduct >>= print
