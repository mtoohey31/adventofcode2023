{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Data.Functor ((<&>))
import GHC.Base (assert)

toRevInts = reverse . map (read :: String -> Int) . words

diffs vss | all (== 0) (head vss) = vss
diffs vss = diffs $ diff (head vss) : vss
  where
    diff (v1 : v2 : vs) = v1 - v2 : diff (v2 : vs)
    diff [_] = []
    diff [] = error "diff of empty list"

extrapolate (vs1 : vs2 : vss) = do
  let (vs2' : vss') = extrapolate (vs2 : vss)
  let vs1Head = head vs1 - head vs2'
  (vs1Head : vs1) : vs2' : vss'
extrapolate [vs] = assert (all (== 0) vs) [0 : vs]

nextValue original = head $ head $ extrapolate $ map reverse $ reverse $ diffs [original]

main = readFile "../../input" <&> lines <&> map (nextValue . toRevInts) <&> sum >>= print
