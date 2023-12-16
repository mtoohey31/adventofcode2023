{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit, ord)
import Data.List (findIndex)
import Data.Text (head, isPrefixOf, reverse, tail)
import Data.Text.IO (hGetLine)
import System.IO (IOMode (ReadMode), hClose, hIsEOF, openFile)
import Prelude hiding (head, reverse, tail)

digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

revDigitWords = map reverse digitWords

firstDigit = firstDigit' digitWords

firstDigit' digitWords s =
  let c = head s
   in if isDigit c
        then ord c - ord '0'
        else case findIndex (`isPrefixOf` s) digitWords of
          Just i -> i + 1
          Nothing -> firstDigit' digitWords $ tail s

lastDigit = firstDigit' revDigitWords . reverse

calibrationSum f = do
  eof <- hIsEOF f
  if eof
    then pure 0
    else do
      l <- hGetLine f
      let tens = firstDigit l
      let ones = lastDigit l
      restSum <- calibrationSum f
      pure $ tens * 10 + ones + restSum

main = do
  f <- openFile "../../input" ReadMode
  answer <- calibrationSum f
  _ <- hClose f
  print answer
