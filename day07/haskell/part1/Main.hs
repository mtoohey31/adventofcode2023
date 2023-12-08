{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}
{-# HLINT ignore "Functor law" #-}

import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (sort, sortBy)
import Data.Map (fromListWith, toList)
import Data.Ord (Down (Down), comparing)

data Card = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | T | J | Q | K | A deriving (Eq, Ord)

toCard c = case c of
  '2' -> N2
  '3' -> N3
  '4' -> N4
  '5' -> N5
  '6' -> N6
  '7' -> N7
  '8' -> N8
  '9' -> N9
  'T' -> T
  'J' -> J
  'Q' -> Q
  'K' -> K
  'A' -> A

data Hand = Hand {cards :: [Card], bid :: Int} deriving (Eq)

instance Ord Hand where
  c1 `compare` c2 = (toKind c1, cards c1) `compare` (toKind c2, cards c2)

toHand s = let [cards, bid] = words s in Hand (map toCard cards) $ read bid

data Kind = High | One | Two | Three | Full | Four | Five deriving (Eq, Ord)

toKind Hand {cards} =
  case sortBy (comparing Down) (map snd $ toList $ fromListWith (+) (map (,1) cards)) of
    [5 :: Int] -> Five
    [4, 1] -> Four
    [3, 2] -> Full
    [3, 1, 1] -> Three
    [2, 2, 1] -> Two
    [2, 1, 1, 1] -> One
    _ -> High

winnings hands = [bid * i | (Hand {bid}, i) <- zip (sort hands) [1 ..]] & sum

main = readFile "../../input" <&> lines <&> map toHand <&> winnings >>= print
