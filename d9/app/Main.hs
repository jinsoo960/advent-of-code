module Main where

import qualified MyLib (someFunc)

getDiff :: [Int] -> [Int] 
getDiff ls = zipWith (-) ls (tail ls)

getDiffs :: [Int] -> [[Int]]
getDiffs ls = if all (== 0) ls then [ls] else ls : getDiffs (getDiff ls)

extrapolate :: [[Int]] -> Int
extrapolate xss = sum (map head xss)

extrapolate' :: [[Int]] -> Int
extrapolate' = foldr ((-) . head) 0 

main :: IO ()
main = do
  f <- readFile "input"
  let ls = map words $ lines f 
  -- let lsint = map (reverse . map read) ls :: [[Int]]
  -- print $ sum $ map (extrapolate . getDiffs) lsint
  let lsint = map (map read) ls :: [[Int]]
  print $ sum $ map (extrapolate . getDiffs) lsint

