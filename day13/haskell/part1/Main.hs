{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <&>" #-}

import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (transpose)
import Data.List.Split (splitOn)

toPatterns input = splitOn "\n\n" input & map toPattern
  where
    toPattern s = lines s & map (map isRock)
    isRock '#' = True
    isRock '.' = False
    isRock _ = undefined

noteSummary pattern = do
  let above = find (mirrorAt pattern) [1 .. length pattern - 1]
  let left = find (mirrorAt (transpose pattern)) [1 .. length (head pattern) - 1]
  case (above, left) of
    (Just above', Nothing) -> above' * 100
    (Nothing, Just left') -> left'
    _ -> undefined
  where
    mirrorAt pattern' nAbove = do
      let (above, below) = splitAt nAbove pattern'
      all (uncurry (==)) $ zip below $ reverse above

main = readFile "../../input" <&> toPatterns <&> map noteSummary <&> sum >>= print
