{-# LANGUAGE NoImplicitPrelude #-}


module Plutus.Crypto.Plonk.Polynomial 
( Polynomial
, evaluatePolynomial
) where

import Plutus.Crypto.BlsField
import PlutusTx.Numeric
import PlutusTx.Foldable

-- [5,3,8] is 8x^2+3x+5, the least significant coefficient is first listed
type Polynomial = [Scalar] 

-- A function to evaluate a polynomial at a given point.
{-# INLINEABLE evaluatePolynomial #-}
evaluatePolynomial :: Polynomial -> Scalar -> Scalar
evaluatePolynomial coeffs x = foldr (\a acc -> a + x * acc) zero coeffs 