module Main where

import qualified MyLib (someFunc)
import Data.Char (digitToInt, isNumber)
import Data.List (reverse, isPrefixOf)
import Control.Monad (msum)

firstDigit :: String -> Int
firstDigit [] = 0
firstDigit (x:xs)
  | isNumber x = digitToInt x
  | otherwise  = firstDigit xs

lastDigit :: String -> Int
lastDigit = firstDigit . reverse

getCalibVal :: String -> Int
getCalibVal s = (firstDigit s) * 10 + (lastDigit s)

numStr = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
numStrRev = map reverse numStr

getPrefixIndex :: [String] -> [String -> Maybe Int]
getPrefixIndex l = map helper (zip l [1..])
  where helper (s, n) = \x -> if isPrefixOf s x then Just n else Nothing 
    
strToidx :: [String -> Maybe Int] -> String -> Maybe Int
strToidx l s = msum (map ($ s) l)

digitLetters :: [String -> Maybe Int] -> String -> Int
digitLetters _ [] = 0
digitLetters fs xs
  | isNumber (head xs) = digitToInt (head xs)
  | otherwise = case strToidx fs xs of
    Just a -> a
    Nothing -> digitLetters fs (tail xs)

firstDigitLetters = digitLetters (getPrefixIndex numStr)
lastDigitLetters = (digitLetters (getPrefixIndex numStrRev)) . reverse

getCalibValLetters :: String -> Int
getCalibValLetters s = (firstDigitLetters s) * 10 + (lastDigitLetters s)

main :: IO ()
main = do
  f <- readFile "input"
  let linesOfFile = lines f
  print (sum (map getCalibValLetters linesOfFile))



