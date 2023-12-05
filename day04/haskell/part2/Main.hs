import Control.Monad.Memo (memo, startEvalMemo)
import Data.Array (bounds, listArray, (!))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import Data.Set (Set, fromList, member)

data Card = Card {winningNumbers :: Set Int, numbers :: [Int]} deriving (Eq, Ord)

toCard line =
  let [_, allNumbers] = splitOn ": " line
   in let [winningNumbers, numbers] = splitOn " | " allNumbers
       in let winningNumbers' = winningNumbers & words & map read & fromList
           in let numbers' = numbers & words & map read
               in Card winningNumbers' numbers'

score Card {winningNumbers, numbers} = filter (`member` winningNumbers) numbers & length

cardCountm (cards, n) = return $ startEvalMemo $ cardCountm' n
  where
    cardCountm' n | n == max = return 1
      where
        (_, max) = bounds cards
    cardCountm' n = do
      won <- mapM (memo cardCountm') [start .. end] <&> sum
      return $ (1 :: Int) + won
      where
        start = n + 1
        (_, max) = bounds cards
        end = (start + score (cards ! n) - 1) `min` max

main = do
  cards <- map toCard . lines <$> readFile "../../input"
  let cardLen = length cards
   in listArray (1, cardLen) cards
        & flip map [1 .. cardLen] . (,)
        & map (startEvalMemo . cardCountm)
        & sum
        & print
