{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Nothing | Just a deriving (Eq)

-- Exercise 2.1 Build a library of things that can fail

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay z (x:xs) = if  z == (fst x) then Just (snd x) else lookupMay z xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (x:xs) = Just (foldl max x xs) 

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (x:xs) = Just (foldl min x xs)

-- Exercise 2.2 Chains of Failing Computations

queryGreek :: GreekData -> String -> Maybe Double
queryGreek greek key = case lookupMay key greek of
    Nothing -> Nothing
    Just xs -> case tailMay xs of
      Nothing -> Nothing
      Just txs -> case maximumMay txs of
        Nothing -> Nothing
        Just mxs -> case headMay xs of
          Nothing -> Nothing
          Just hxs -> divMay (fromIntegral mxs) (fromIntegral hxs)

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just a) = f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link Nothing f = Nothing
link (Just a) f = f a

-- TODO
-- queryGreek2 :: GreekData -> String -> Maybe Double

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaries p1 p2 =
    case (lookupMay p1 salaries, lookupMay p2 salaries) of
        (Just s1, Just s2) -> Just (s1 + s2)
        _ -> Nothing

