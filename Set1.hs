{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

-- Exercise 1.1 Random Number Generation

fiveRands :: [Integer]
fiveRands = [e1, e2, e3, e4, e5]
          where
            s0 = mkSeed 1
            (e1, s1) = rand s0
            (e2, s2) = rand s1
            (e3, s3) = rand s2
            (e4, s4) = rand s3
            (e5, _)  = rand s4

-- Exercise 1.2 Random Character Generation

randLetter :: Seed -> (Char, Seed)
randLetter s = (toLetter c, s')
             where
               (c, s') = rand s

randString3 :: String
randString3 = [c1, c2, c3]
           where 
             s0 = mkSeed 1
             (c1, s1) = randLetter s0
             (c2, s2) = randLetter s1
             (c3, s3) = randLetter s2

-- Exercise 1.3 More Generators

type Gen a = Seed -> (a, Seed)

{-
 -
 - Without generalA

randEven :: Gen Integer
randEven s = even 
    where 
       (num, seed) = rand s 
       even = (num * 2, seed)

randOdd :: Gen Integer
randOdd s = odd 
    where
      (num, seed) = randEven s
      odd  = (num+1, seed)

randTen :: Gen Integer
randTen s = ten
     where
       (num, seed) = rand s
       ten = num * 10 
--} 

generalA :: (a -> b) -> Gen a -> Gen b
generalA f gen seed = (result, seed2)
     where 
       (x, seed2) = gen seed
       result =  f x

randEven :: Gen Integer
randEven = generalA (\x -> x * 2) rand

randOdd :: Gen Integer
randOdd = generalA (\x -> x + 1) randEven

randTen :: Gen Integer
randTen = generalA (\x -> x + 10) rand


-- Exercise 1.4 Generalizing Random Pairs


randPair :: Gen (Char, Integer) --- randPair (mkSeed 1) output: ('l', 282475249)
randPair seed = result
     where
       (letter, s)  = randLetter seed
       (num, s2) = rand s
       result = ((letter, num), s2)

generalPair :: Gen a -> Gen b -> Gen (a,b) -- generalPair :: (Seed -> (a, Seed)) -> (Seed -> (b, Seed)) -> (Seed -> ((a,b), Seed))
generalPair f g s = ((a,b), s2)
     where 
        (a, s1) = f s 
        (b, s2) = g s1

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb s = (f a b, s2)
      where 
        (a, s1) = ga s
        (b, s2) = gb s1

-- Exercise 1.5 Generalizing Lists of Generators

-- type Gen a = Seed -> (a, Seed)

repRandom :: [Gen a] -> Gen [a]
--repRandom :: [Seed -> (a, Seed)] -> (Seed -> ([a], Seed))
repRandom [] s = ([], s)
repRandom (x:xs) s = (y : ys, s'')
  where
    (y, s') = x s
    (ys, s'') = repRandom xs s'

-- Exercise 1.6 Threading the random number state

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g f s = (x', s'')
  where
    (x, s') = g s
    (x', s'') = f x s'

mkGen :: a -> Gen a
mkGen x = \s -> (x, s)
