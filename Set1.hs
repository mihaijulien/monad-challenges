{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

fiveRands :: [Integer]
fiveRands = [e1, e2, e3, e4, e5]
          where
            s0 = mkSeed 1
            (e1, s1) = rand s0
            (e2, s2) = rand s1
            (e3, s3) = rand s2
            (e4, s4) = rand s3
            (e5, _)  = rand s4

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
