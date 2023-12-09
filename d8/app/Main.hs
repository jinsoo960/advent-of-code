module Main where

import qualified MyLib (someFunc)

import qualified Data.Map as M
import Data.List

parseLine :: String -> (String, (String, String))
parseLine s = (reverse $ head ws, (reverse $ tail (take 4 (ws !! 2)), reverse $ take 3 (ws !! 3)))
  where ws =  words s

followInstructions :: String -> M.Map String (String, String) -> String -> Int
followInstructions (c:is) m current = if head current == 'Z' then 0 else 1 + followInstructions is m (next $ m M.! current)
  where next = if c == 'L' then fst else snd
followInstructions _ _ _ = undefined

main :: IO ()
main = do
  f <- readFile "input"
  let ls = lines f
  let instr = head ls
  let m = M.fromList $ map parseLine $ drop 2 ls
  let starts = filter (\x -> head x == 'A') $ M.keys m
  let results = map (followInstructions (cycle instr) m) starts
  print $ foldl lcm 1 results

