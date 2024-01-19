{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# LANGUAGE ImportQualifiedPost   #-}

module Plutus.Crypto.BlsField
( bls12_381_field_prime
, Scalar (..)
, mkScalar
, MultiplicativeGroup (..)
, powerOfTwoExponentiation
, reverseByteString
) where

import PlutusTx (makeIsDataIndexed, makeLift, unstableMakeIsData)
import PlutusTx.Builtins (BuiltinBLS12_381_G1_Element, BuiltinBLS12_381_G2_Element,
                          BuiltinByteString, bls12_381_G1_add, bls12_381_G1_compressed_zero,
                          bls12_381_G1_equals, bls12_381_G1_neg, bls12_381_G1_scalarMul,
                          bls12_381_G1_uncompress, bls12_381_G2_add, bls12_381_G2_compressed_zero,
                          bls12_381_G2_neg, bls12_381_G2_scalarMul, bls12_381_G2_uncompress,
                          consByteString, emptyByteString, indexByteString, integerToByteString,
                          lengthOfByteString)
import PlutusTx.Numeric (AdditiveGroup (..), AdditiveMonoid (..), AdditiveSemigroup (..),
                         Module (..), MultiplicativeMonoid (..), MultiplicativeSemigroup (..))
import PlutusTx.Prelude (AdditiveGroup (..), AdditiveMonoid (..), AdditiveSemigroup (..), Eq (..),
                         Integer, Module (..), MultiplicativeMonoid (..),
                         MultiplicativeSemigroup (..), Ord (..), divide, dropByteString, error,
                         even, modulo, otherwise, ($), (&&), (<>))
import Prelude qualified as Haskell

-- In this module, we create a prime order field for BLS12-381
-- as the type Scalar. Note that for safety, the Scalar constructors
-- are not exposed. Instead, the mkScalar and unScalar suffice,
-- which fail in a script if an integer provided that is negative.

-- The prime order of the generator in the field. So, g^order = id,
-- or more general, for any element x in the field, x^order = id.
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
-- We also implement the reciprocal (the multiplicative inverse of an element in the group).
-- For the additive group, there is negate function in PlutusTx.Numeric.
class MultiplicativeMonoid a => MultiplicativeGroup a where
    div :: a -> a -> a
    recip :: a -> a

-- Reverse a builtin byte string of arbitrary length
-- This can convert between little and big endian.
{-# INLINABLE reverseByteString #-}
reverseByteString :: BuiltinByteString -> BuiltinByteString
reverseByteString bs
    | bs == emptyByteString = bs
    | otherwise             = reverseByteString (dropByteString 1 bs) <> consByteString (indexByteString bs 0) emptyByteString

-- this one costs around 12.1% of cpu budget
-- while bitshifts modExp cost around 9.6%
{-# INLINABLE powMod #-}
powMod :: Scalar -> Integer -> Scalar
powMod b e
    | e < 0     = zero
    | e == 0    = one
    | even e   = powMod (b*b) (e `divide` 2)
    | otherwise = b * powMod (b*b) ((e - 1) `divide` 2)

-- In math this is b^a mod p, where b is of type scalar and a any integer
-- note that there is still some overhead here due to the conversion from
-- little endian to big endian (and bs <-> integer). This can be
-- optimized in the future.
instance Module Integer Scalar where
    {-# INLINABLE scale #-}
    scale :: Integer -> Scalar -> Scalar
    scale a b = powMod b a

instance MultiplicativeGroup Scalar where
    {-# INLINABLE div #-}
    div a b | b == Scalar 0 = error ()
            | otherwise     = a * scale (bls12_381_field_prime - 2) b -- use Fermat little theorem
    {-# INLINABLE recip #-}
    recip = div one

-- This is a special case of modular exponentiation, where the exponent is a power of two.
-- This saves alot of script budget. Note that for x^e,  e = 2^k, and k is used below
{-# INLINABLE powerOfTwoExponentiation #-}
powerOfTwoExponentiation :: Scalar -> Integer -> Scalar
powerOfTwoExponentiation x k = if k < 0 then error () else go x k
    where go x' k'
            | k' == 0    = x'
            | otherwise = powerOfTwoExponentiation (x'*x') (k' - 1)

-- Add instance to make G1 and G2 a AdditiveGroup + scale function

instance AdditiveSemigroup BuiltinBLS12_381_G1_Element where
    {-# INLINABLE (+) #-}
    (+) = bls12_381_G1_add

instance AdditiveMonoid BuiltinBLS12_381_G1_Element where
    {-# INLINABLE zero #-}
    zero = bls12_381_G1_uncompress bls12_381_G1_compressed_zero

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
    zero = bls12_381_G2_uncompress bls12_381_G2_compressed_zero

instance AdditiveGroup BuiltinBLS12_381_G2_Element where
    {-# INLINABLE (-) #-}
    (-) a b = a + bls12_381_G2_neg b

instance Module Scalar BuiltinBLS12_381_G2_Element where
    {-# INLINABLE scale #-}
    scale (Scalar a) = bls12_381_G2_scalarMul a
