{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a, b)] -- allPairs l1 l2 = [(x,y) | x <- l1, y <- l2]
allPairs [] _ = []
allPairs _ [] = []
allPairs (x:xs) l2 = makePairs x l2 ++ allPairs xs l2
    where 
      makePairs :: a -> [b] -> [(a,b)]
      makePairs _ [] = []
      makePairs num (y:ys) = [(num, y)] ++ makePairs num ys


-- allPairs cardRanks cardSuits == [(2,"H"),(2,"D"),(2,"C"),(2,"S"),(3,"H"),(3,"D"),(3,"C"),(3,"S"),(4,"H"),(4,"D"),(4,"C"),(4,"S"),(5,"H"),(5,"D"),(5,"C"),(5,"S")]

data Card = Card Int String 

instance Show Card where
    show :: Card -> String
    show (Card rank suit) = show rank ++ suit


allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards _ [] = []
allCards (x:xs) cardSuits = makePairs x cardSuits ++ allCards xs cardSuits
     where
       makePairs :: Int -> [String] -> [Card]
       makePairs _ [] = []
       makePairs cardRank (cardSuit:restCardSuits) = [Card cardRank cardSuit] ++ makePairs cardRank restCardSuits

-- show (allCards cardRanks cardSuits) == "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]"

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs _ _ [] = []
allCombs f (x:xs) l2 = makePairs x l2 ++ allCombs f xs l2
    where
      makePairs _ [] = []
      makePairs x (y:ys) = f x y : makePairs x ys


allPairs2 :: [a] -> [b] -> [(a,b)]
allPairs2 l1 l2 = allCombs (\x y -> (x, y)) l1 l2

-- allPairs2 [1, 2] ['a', 'b'] == [(1,'a'), (1,'b'), (2,'a'), (2,'b')]

allCards2 :: [Int] -> [String] -> [Card]
allCards2 l1 l2 = allCombs (\x y -> Card x y) l1 l2

-- allCards2 cardRanks cardSuits == [(2,"H"),(2,"D"),(2,"C"),(2,"S"),(3,"H"),(3,"D"),(3,"C"),(3,"S"),(4,"H"),(4,"D"),(4,"C"),(4,"S"),(5,"H"),(5,"D"),(5,"C"),(5,"S")]
