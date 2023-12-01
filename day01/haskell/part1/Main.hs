import Data.Char (isDigit, ord)
import Data.Text (filter, head, last)
import Data.Text.IO (hGetLine)
import System.IO (IOMode (ReadMode), hClose, hIsEOF, openFile)
import Prelude hiding (filter, head, last)

parseDigitUnsafe c = ord c - ord '0'

calibrationSum f = do
  eof <- hIsEOF f
  if eof
    then pure 0
    else do
      l <- hGetLine f
      let tens = parseDigitUnsafe $ head $ filter isDigit l
      let ones = parseDigitUnsafe $ last $ filter isDigit l
      restSum <- calibrationSum f
      pure $ tens * 10 + ones + restSum

main = do
  f <- openFile "../../input" ReadMode
  answer <- calibrationSum f
  _ <- hClose f
  print answer
