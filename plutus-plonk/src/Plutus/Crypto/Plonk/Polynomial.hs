{-# LANGUAGE NoImplicitPrelude #-}


module Plutus.Crypto.Plonk.Polynomial 
( Polynomial
, evaluatePolynomial
) where

import Plutus.Crypto.BlsField
import PlutusTx.Numeric
import PlutusTx.Foldable

-- TODO: see what operation besides evaluation we need onchain.

type Polynomial = [Scalar] -- [5,3,8] is 8x^2+3x+5, the least significant coefficient is first

{-# INLINEABLE evaluatePolynomial #-}
evaluatePolynomial :: Polynomial -> Scalar -> Scalar
evaluatePolynomial coeffs x = foldr (\a acc -> a + x * acc) zero coeffs 