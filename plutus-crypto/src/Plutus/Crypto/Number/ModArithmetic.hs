{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Crypto.Number.ModArithmetic 
( exponentiate
, exponentiateMod
) where

import PlutusTx
import PlutusTx.Prelude

-- | Exponentiate x^n. 
-- This function will give an error for negative integers.
{-# INLINABLE exponentiate #-}
exponentiate :: Integer -> Integer -> Integer
exponentiate x n
    | n < 0 || x < 0    = error ()
    | n == 0            = 1
    | x == 0            = 0
    | even n            = exponentiate x (n `divide` 2) * exponentiate x (n `divide` 2)
    | otherwise         = x * exponentiate x ((n - 1) `divide` 2) * exponentiate x ((n - 1) `divide` 2)

-- | Exponentiate b^e mod m.
-- This function will give an error for negative integers.
{-# INLINABLE exponentiateMod #-}
exponentiateMod :: Integer -> Integer -> Integer -> Integer
exponentiateMod b e m
    | b < 0 || e < 0 || m < 0 = error ()
    | b == 1    = b
    | e == 0    = 1
    | e == 1    = b `modulo` m
    | even e    = let p = exponentiateMod b (e `divide` 2) m `modulo` m
                   in exponentiate p 2 `modulo` m
    | otherwise = (b * exponentiateMod b (e - 1) m) `modulo` m