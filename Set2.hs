{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

-- Exercise 2.1 The Maybe Type

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
  -- show :: Maybe a -> String
  show Nothing = "Nothing"
  show (Just x) = "Just " ++ show x

instance Eq a => Eq (Maybe a) where
  -- (==) :: Maybe a -> Maybe a -> Bool
  (==) Nothing Nothing = True
  (==) (Just x) (Just y) = x == y
  (==) _ _ = False

-- Exercise 2.2 Build a library of things that can fail

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

-- Exercise 2.3 Chains of Failing Computations

-- type GreekData = [(String, [Integer])]

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

-- Exercise 2.4 Generalizing chains of failures

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just a) = f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
--link Nothing f = Nothing
--link (Just a) f = f a
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 greek key = lookupMay key greek `link` \xs ->
    tailMay xs `link` \tl ->
      maximumMay tl `link` \mx ->
        headMay xs `link` \hd -> divMay (fromIntegral mx) (fromIntegral hd)

-- Exercise 2.5 Chaining variations

{-
salaries :: [(String, Integer)]
salaries = [ ("alice", 105000)
           , ("bob", 90000)
           , ("carol", 85000)
           ]
-}

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries salaries p1 p2 =
    case (lookupMay p1 salaries, lookupMay p2 salaries) of
        (Just s1, Just s2) -> Just (s1 + s2)
        _ -> Nothing

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f Nothing _ = Nothing
yLink f _ Nothing = Nothing
yLink f (Just a) (Just b) = Just (f a b)

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 salaries p1 p2 = yLink (+) (lookupMay p1 salaries) (lookupMay p2 salaries)

-- Exercise 2.6 Tailprod

tailProd :: Num a => [a] -> Maybe a
tailProd [] = Nothing
tailProd xs = tailMay xs `link` (Just . product)

tailSum :: Num a => [a] -> Maybe a
tailSum [] = Nothing
tailSum xs = tailMay xs `link` (Just . sum)

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe _ Nothing = Nothing
transMaybe f (Just a) = Just (f a)

tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax =  transMaybe maximumMay . tailMay 

tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin =  transMaybe minimumMay . tailMay 

combine :: Maybe (Maybe a) -> Maybe a
combine Nothing = Nothing
combine (Just Nothing) = Nothing
combine (Just (Just a)) = Just a

