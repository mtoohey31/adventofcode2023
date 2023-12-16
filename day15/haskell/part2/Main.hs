{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Control.Arrow ((>>>))
import Data.Array (listArray, (!), (//))
import Data.Array qualified
import Data.Char (ord)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)
import Data.Map.Ordered (delete, (|>))
import Data.Map.Ordered qualified

data Op = Remove | Add Int

toSteps = lines >>> concat >>> splitOn "," >>> map toStep
  where
    toStep s =
      if "-" `isSuffixOf` s
        then (init s, Remove)
        else do
          let [label, focalLength] = splitOn "=" s
          (label, Add $ read focalLength)

hash = foldl update (0 :: Int)
  where
    update n c = (n + ord c) * 17 `rem` 256

finalBoxes = foldl doStep (listArray (0, 255) $ repeat Data.Map.Ordered.empty)
  where
    doStep boxes (label, op) = boxes // [(boxNumber, updatedBox)]
      where
        boxNumber = hash label
        updatedBox = case op of
          Remove -> delete label $ boxes ! boxNumber
          Add focalLength -> boxes ! boxNumber |> (label, focalLength)

focusingPower = Data.Array.assocs >>> map focusingPower' >>> sum
  where
    focusingPower' (i, box) =
      Data.Map.Ordered.assocs box
        & map snd
        & zipWith (*) [1 ..]
        & sum
        & (* (i + 1))

main = readFile "../../input" <&> toSteps <&> finalBoxes <&> focusingPower >>= print
