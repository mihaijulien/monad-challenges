{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set2

-- Exercise 4.1 Generalizing State and Maybe
{-
Set1 functions:                                           Set2 functions:

fiveRands :: [Integer]                                    headMay :: [a] -> Maybe a
randLetter :: Seed -> (Char, Seed)                        tailMay :: [a] -> Maybe [a]
randString3 :: String                                     lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
generalA :: (a -> b) -> Gen a -> Gen b                    divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
randEven :: Gen Integer                                   maximumMay :: Ord a => [a] -> Maybe a
randOdd :: Gen Integer                                    minimumMay :: Ord a => [a] -> Maybe a
randTen :: Gen Integer                                    queryGreek :: GreekData -> String -> Maybe Double
randPair :: Gen (Char, Integer)                           chain :: (a -> Maybe b) -> Maybe a -> Maybe b
generalPair :: Gen a -> Gen b -> Gen (a,b)                link :: Maybe a -> (a -> Maybe b) -> Maybe b
generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c      queryGreek2 :: GreekData -> String -> Maybe Double
repRandom :: [Gen a] -> Gen [a]                           addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
genTwo :: Gen a -> (a -> Gen b) -> Gen b                  yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
mkGen :: a -> Gen a                                       tailProd :: Num a => [a] -> Maybe a
                                                          tailSum :: Num a => [a] -> Maybe a
                                                          transMaybe :: (a -> b) -> Maybe a -> Maybe b
                                                          tailMax :: Ord a => [a] -> Maybe (Maybe a)
                                                          tailMin :: Ord a => [a] -> Maybe (Maybe a)
                                                          combine :: Maybe (Maybe a) -> Maybe a

Similar type signature:

(Set1) generalB & (Set2) yLink
----------------------------------
generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c

(Set1) genTwo & (Set2) link
---------------------------------
genTwo :: Gen a -> (a -> Gen b) -> Gen b
link :: Maybe a -> (a -> Maybe b) -> Maybe b

-}



-- pattern1 :: m a -> (a -> m b) -> m b
-- pattern2 :: (a -> b -> c) -> m a -> m b -> m c

-- Exercise 4.3 Formalizing the Pattern

class Monad m where  
    bind :: m a -> (a -> m b) -> m b 
    return :: a -> m a 


-- Exercise 4.4 Creating Instances

{-
brush up your knowledge:
newtype: https://learnyouahaskell.com/functors-applicative-functors-and-monoids#the-newtype-keyword
typeclasses: https://learnyouahaskell.com/making-our-own-types-and-typeclasses#typeclasses-102
-} 

newtype Gen a =  Gen { runGen :: Seed -> (a, Seed) }

evalGen :: Gen a -> Seed -> a
evalGen gen s = fst (runGen gen s)

instance Monad Gen where
    bind (Gen g) f = Gen $ \s ->
            let (a, s') = g s
                Gen g' = f a
            in g' s'
    return a = Gen $ \s -> (a, s)
