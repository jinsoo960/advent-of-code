module Main where

import qualified MyLib (someFunc)

import qualified Data.Map as M
import Data.Sort
import Data.Ord


countElems :: (Ord a) => [a] -> M.Map a Int
countElems = M.fromListWith (+) . flip zip (repeat 1)


data Card = CardJ | Card2 | Card3 | Card4 | Card5 | Card6 | Card7 | Card8 | Card9 | CardT | CardQ | CardK | CardA
  deriving (Eq, Ord, Show, Read)


newtype CardCount = CardCount (M.Map Card Int)
  deriving (Eq, Show)


data HandRank = HighCard | OnePair | TwoPair | ThreeOK | FullHouse | FourOK | FiveOK 
  deriving (Eq, Ord, Show)


cctoHR :: CardCount -> HandRank
cctoHR (CardCount m) = case sort (M.elems m) of
  [5] -> FiveOK
  [1, 4] -> FourOK
  [2, 3] -> FullHouse
  [1, 1, 3] -> ThreeOK
  [1, 2, 2] -> TwoPair
  [1, 1, 1, 2] -> OnePair
  _ -> HighCard


cctoHR' :: CardCount -> HandRank
cctoHR' (CardCount m) = case m M.!? CardJ of
  Just 5 -> FiveOK
  Just n -> let m' = M.delete CardJ m
                mostCommon = fst $ head $ sortOn (Down . snd) (M.assocs m')
            in cctoHR $ CardCount $ M.adjust (+ n) mostCommon m' 
  Nothing -> cctoHR (CardCount m)


instance Ord CardCount where
  c1 `compare` c2 = cctoHR' c1 `compare` cctoHR' c2


data Hand = Hand CardCount [Card] 
  deriving (Eq, Show)

instance Ord Hand where
  (Hand cc1 cl1) `compare` (Hand cc2 cl2) = case cc1 `compare` cc2 of
    EQ -> cl1 `compare` cl2
    x -> x


charToCard :: Char -> Card
charToCard c = read $ "Card" ++ [c]


stringToHand :: String -> Hand
stringToHand s = let cl = map charToCard s in Hand (CardCount (countElems cl)) cl


lineToHandBid :: String -> (Hand, Int)
lineToHandBid s = let ss = words s in
  (stringToHand (head ss), read (ss !! 1))


main :: IO ()
main = do
  f <- readFile "input"
  let ls = lines f
  let handsBid = map lineToHandBid ls
  print $ foldl (\y ((_, bid), r) -> y + bid * r) 0 $ zip (sort handsBid) [1..]
