{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module Plutus.Crypto.BlsField
( bls12_381_field_prime
, Scalar
, mkScalar
, unScalar
, MultiplicativeGroup (..)
, modularExponentiationScalar
, powerOfTwoExponentiation
) where

import qualified Prelude as Haskell
import PlutusTx.Prelude
    ( otherwise,
      Integer,
      ($),
      (&&),
      error,
      modulo,
      Eq(..),
      AdditiveGroup(..),
      AdditiveMonoid(..),
      AdditiveSemigroup(..),
      Module(..),
      MultiplicativeMonoid(..),
      MultiplicativeSemigroup(..),
      Ord((<), (<=)) )
import PlutusTx (makeLift, makeIsDataIndexed, unstableMakeIsData)
import PlutusTx.Numeric
    ( AdditiveGroup(..),
      AdditiveMonoid(..),
      AdditiveSemigroup(..),
      Module(..),
      MultiplicativeMonoid(..),
      MultiplicativeSemigroup(..) )
import Plutus.Crypto.Number.ModArithmetic ( exponentiateMod )
import PlutusTx.Builtins
    ( popCountByteString,
      bls12_381_G1_equals,
      BuiltinBLS12_381_G1_Element,
      bls12_381_G1_add,
      bls12_381_G1_zero,
      bls12_381_G1_neg,
      bls12_381_G1_scalarMul,
      BuiltinBLS12_381_G2_Element,
      bls12_381_G2_add,
      bls12_381_G2_scalarMul,
      bls12_381_G2_neg,
      bls12_381_G2_zero,
      BuiltinByteString,
      shiftByteString,
      testBitByteString,
      lengthOfByteString )



-- In this module, we create a prime order field for BLS12-381
-- as the type Scalar.
-- Note that for safety, the Scalar constructors are not exposed.
-- Instead, the mkScalar and unScalar suffice, which fail in a script
-- if an integer provided that is negative

-- this is the order of the generator. So, g^order = id
bls12_381_field_prime :: Integer
bls12_381_field_prime = 52435875175126190479447740508185965837690552500527637822603658699938581184513

newtype Scalar = Scalar { unScalar :: Integer} deriving (Haskell.Show)
makeLift ''Scalar
makeIsDataIndexed ''Scalar [('Scalar,0)]

-- Exclude for safety negative integers and integers large/equal
-- to the field prime. This is the primary interface to work with
-- the Scalar type onchain. This is for security reasons 
-- (to make sure they are field elements).
{-# INLINABLE mkScalar #-}
mkScalar :: Integer -> Scalar
mkScalar n | 0 <= n && n < bls12_381_field_prime = Scalar n
           | otherwise                           = error ()

instance Eq Scalar where
    {-# INLINABLE (==) #-}
    Scalar a == Scalar b = a == b

instance AdditiveSemigroup Scalar where
    {-# INLINABLE (+) #-}
    (+) (Scalar a) (Scalar b) = Scalar $ (a+b) `modulo` bls12_381_field_prime

instance AdditiveMonoid Scalar where
    {-# INLINABLE zero #-}
    zero = Scalar 0

instance AdditiveGroup Scalar where
    {-# INLINABLE (-) #-}
    (-) (Scalar a) (Scalar b) = Scalar $ (a-b) `modulo` bls12_381_field_prime

instance MultiplicativeSemigroup Scalar where
    {-# INLINABLE (*) #-}
    (*) (Scalar a) (Scalar b) = Scalar $ (a*b) `modulo` bls12_381_field_prime

instance MultiplicativeMonoid Scalar where
    {-# INLINABLE one #-}
    one = Scalar 1

-- In plutus 1.9, PlutusTx.Numeric does not implement a Multiplicative group.
-- But since we use a field, inversion is well-defined if we exclude 0.
-- Also implement the reciprocal (the multiplicative inverse of an element in the group)
-- For the additive group, there is negate function in PlutusTx.Numeric.
class MultiplicativeMonoid a => MultiplicativeGroup a where
    div :: a -> a -> a
    recip :: a -> a

-- this function is broken as this assumes big endian and shifts have
-- the behavior of shifting bit across bytes.
{-# NOINLINE modularExponentiationScalar #-}
modularExponentiationScalar :: Scalar -> BuiltinByteString -> Scalar
modularExponentiationScalar b e
    | popCountByteString e == 0  = one
    | otherwise = t * modularExponentiationScalar (b*b) (shiftByteString e 1)
                where t = if testBitByteString e 0 then b else one

-- this is just a wrapped exponentiateMod for the field elements
-- In math this is b^a mod p, where b is of type scalar and a any integer
instance Module Integer Scalar where
    {-# INLINABLE scale #-}
    scale :: Integer -> Scalar -> Scalar
    scale a (Scalar b) = Scalar (exponentiateMod b a bls12_381_field_prime)

instance MultiplicativeGroup Scalar where
    {-# INLINABLE div #-}
    div a b | b == Scalar 0 = error ()
            | otherwise     = a * scale (bls12_381_field_prime - 2) b -- use Fermat little theorem
    {-# INLINABLE recip #-}
    recip = div one

-- Add instance to make G1 and G2 a AdditiveGroup + scale function

instance AdditiveSemigroup BuiltinBLS12_381_G1_Element where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G1_add

instance AdditiveMonoid BuiltinBLS12_381_G1_Element where
    {-# INLINABLE zero #-}
    zero = bls12_381_G1_zero

instance AdditiveGroup BuiltinBLS12_381_G1_Element where
    {-# INLINABLE (-) #-}
    (-) a b = a + bls12_381_G1_neg b

instance Module Scalar BuiltinBLS12_381_G1_Element where
    {-# INLINABLE scale #-}
    scale (Scalar a) = bls12_381_G1_scalarMul a

instance AdditiveSemigroup BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G2_add

instance AdditiveMonoid BuiltinBLS12_381_G2_Element where
    {-# INLINABLE zero #-}
    zero = bls12_381_G2_zero

instance AdditiveGroup BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (-) #-}
    (-) a b = a + bls12_381_G2_neg b

instance Module Scalar BuiltinBLS12_381_G2_Element where
    {-# INLINABLE scale #-}
    scale (Scalar a) = bls12_381_G2_scalarMul a

{-# INLINABLE powerOfTwoExponentiation #-}
powerOfTwoExponentiation :: Scalar -> Integer -> Scalar
powerOfTwoExponentiation x k = if k < 0 then error () else go x k
    where go x' k'
            | k' == 0    = x'
            | otherwise = powerOfTwoExponentiation (x'*x') (k' - 1)