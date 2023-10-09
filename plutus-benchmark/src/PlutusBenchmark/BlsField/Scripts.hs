{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module PlutusBenchmark.BlsField.Scripts
( blsFieldAddScalarsScript
, blsFieldMulScalarsScript
, listOfSizedByteStrings
, modularExponentiationScalarScript
, modularExponentiationScalarScript2
, modExpPow2Script
) where

import PlutusTx (compile, unsafeApplyCode, liftCodeDef, getPlcNoAnn)
import PlutusTx.Prelude (Integer, ($), (-), (.), foldr, (*), BuiltinByteString, popCountByteString, (==), otherwise, byteStringToInteger, integerToByteString, (<>), modulo, divide, (<))
import PlutusTx.Numeric ((+), zero, one)

import PlutusCore (DefaultFun, DefaultUni)
import UntypedPlutusCore qualified as UPLC

import Prelude qualified as Haskell
import Hedgehog.Internal.Gen qualified as G
import Hedgehog.Internal.Range qualified as R
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString (ByteString)
import PlutusTx.Builtins ( testBitByteString, shiftByteString, error )
import Plutus.Crypto.BlsField (Scalar (unScalar), mkScalar)
import Plutus.Crypto.Number.Serialize (nullPadding)

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

{-# NOINLINE modularExponentiationScalar #-}
modularExponentiationScalar :: Scalar -> BuiltinByteString -> Scalar
modularExponentiationScalar b e
    | popCountByteString e == 0  = one
    | otherwise = t * modularExponentiationScalar (b*b) (shiftByteString e 1)
        where t = if testBitByteString e 0 then b else one

modularExponentiationScalarScript :: Scalar -> BuiltinByteString -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
modularExponentiationScalarScript b e =
    getPlcNoAnn $ $$(compile [|| modularExponentiationScalar ||])
        `unsafeApplyCode` liftCodeDef b
        `unsafeApplyCode` liftCodeDef e

{-# NOINLINE modularExponentiationScalar2 #-}
modularExponentiationScalar2 :: Scalar -> Integer -> Scalar
modularExponentiationScalar2 b power
    | power == 0  = b
    | otherwise = mkScalar (byteStringToInteger ((\x -> nullPadding power <> x) (integerToByteString (unScalar b ))) `modulo` 52435875175126190479447740508185965837690552500527637822603658699938581184513)

modularExponentiationScalarScript2 :: Scalar -> Integer -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
modularExponentiationScalarScript2 b pow =
    getPlcNoAnn $ $$(compile [|| modularExponentiationScalar2 ||])
        `unsafeApplyCode` liftCodeDef b
        `unsafeApplyCode` liftCodeDef pow

{-# NOINLINE powerOfTwoExponentiation #-}
powerOfTwoExponentiation :: Scalar -> Integer -> Scalar
powerOfTwoExponentiation x k = if k < 0 then error () else go x k
    where go x' k'
            | k' == 0    = x'
            | otherwise = powerOfTwoExponentiation (x'*x') (k' - 1)

modExpPow2Script :: Scalar -> Integer -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
modExpPow2Script b pow =
    getPlcNoAnn $ $$(compile [|| powerOfTwoExponentiation ||])
        `unsafeApplyCode` liftCodeDef b
        `unsafeApplyCode` liftCodeDef pow