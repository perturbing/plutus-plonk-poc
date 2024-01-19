{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module PlutusBenchmark.BlsField.Scripts
( blsFieldAddScalarsScript
, blsFieldMulScalarsScript
, listOfSizedByteStrings
, modularExponentiationScalarScript
, modExpPow2Script
, invertScalarsScript
) where

import PlutusTx (compile, getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import PlutusTx.Numeric (one, scale, zero, (+))
import PlutusTx.Prelude (Integer, foldr, map, ($), (*), (.))

import PlutusCore (DefaultFun, DefaultUni)
import UntypedPlutusCore qualified as UPLC

import Data.ByteString (ByteString)
import Hedgehog.Internal.Gen qualified as G
import Hedgehog.Internal.Range qualified as R
import Plutus.Crypto.BlsField (MultiplicativeGroup (..), Scalar, powerOfTwoExponentiation)
import Prelude qualified as Haskell
import System.IO.Unsafe (unsafePerformIO)

{-# INLINABLE addScalars #-}
addScalars :: [Scalar] -> Scalar
addScalars = foldr (+) zero

blsFieldAddScalarsScript :: [Scalar] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
blsFieldAddScalarsScript xs =
    getPlcNoAnn $ $$(compile [|| addScalars ||])
       `unsafeApplyCode` liftCodeDef xs

{-# INLINABLE mulScalars #-}
mulScalars :: [Scalar] -> Scalar
mulScalars = foldr (*) one

blsFieldMulScalarsScript :: [Scalar] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
blsFieldMulScalarsScript xs =
    getPlcNoAnn $ $$(compile [|| mulScalars ||])
       `unsafeApplyCode` liftCodeDef xs

{-# NOINLINE listOfSizedByteStrings #-}
listOfSizedByteStrings :: Integer -> Integer -> [ByteString]
listOfSizedByteStrings n l = unsafePerformIO . G.sample $
                             G.list (R.singleton $ Haskell.fromIntegral n)
                                  (G.bytes (R.singleton $ Haskell.fromIntegral l))

{-# INLINABLE modularExponentiationList #-}
modularExponentiationList :: [Scalar] -> Integer -> [Scalar]
modularExponentiationList xs e = map (scale e) xs

modularExponentiationScalarScript :: [Scalar] -> Integer -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
modularExponentiationScalarScript xs e =
    getPlcNoAnn $ $$(compile [|| modularExponentiationList ||])
        `unsafeApplyCode` liftCodeDef xs
        `unsafeApplyCode` liftCodeDef e

modExpPow2Script :: Scalar -> Integer -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
modExpPow2Script b pow =
    getPlcNoAnn $ $$(compile [|| powerOfTwoExponentiation ||])
        `unsafeApplyCode` liftCodeDef b
        `unsafeApplyCode` liftCodeDef pow

{-# INLINABLE invertScalars #-}
invertScalars :: [Scalar] -> Scalar
invertScalars = foldr (\a b -> b + recip a) zero

invertScalarsScript :: [Scalar] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
invertScalarsScript xs =
    getPlcNoAnn $ $$(compile [|| invertScalars ||])
       `unsafeApplyCode` liftCodeDef xs
