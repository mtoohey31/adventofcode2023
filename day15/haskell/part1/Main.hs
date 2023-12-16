{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Control.Arrow ((>>>))
import Data.Char (ord)
import Data.Functor ((<&>))
import Data.List.Split (splitOn)

toSteps = lines >>> concat >>> splitOn ","

hash = foldl update (0 :: Int)
  where
    update n c = (n + ord c) * 17 `rem` 256

main = readFile "../../input" <&> toSteps <&> map hash <&> sum >>= print
