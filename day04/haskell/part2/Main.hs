import Data.Array (bounds, listArray, (!))
import Data.Function ((&))
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)

data Card = Card {winningNumbers :: Set Int, numbers :: [Int]}

toCard line =
  let [_, allNumbers] = splitOn ": " line
   in let [winningNumbers, numbers] = splitOn " | " allNumbers
       in let winningNumbers' = winningNumbers & words & map read & fromList
           in let numbers' = numbers & words & map read
               in Card winningNumbers' numbers'

score Card {winningNumbers, numbers} = filter (`member` winningNumbers) numbers & length

cardCount cards (n : ns) =
  (1 :: Int) + cardCount cards ([start .. end] ++ ns)
  where
    start = n + 1
    (_, max) = bounds cards
    end = (start + score (cards ! n) - 1) `min` max
cardCount _ [] = 0

main = do
  cards <- map toCard . lines <$> readFile "../../input"
  let cardLen = length cards
   in listArray (1, length cards) cards & flip cardCount [1 .. cardLen] & print
