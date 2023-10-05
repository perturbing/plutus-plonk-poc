{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module PlutusBenchmark.BlsField.Scripts
( blsFieldAddScalarsScript
, blsFieldMulScalarsScript
, listOfSizedByteStrings
, modularExponentiationScalarScript
) where

import PlutusTx (compile, unsafeApplyCode, liftCodeDef, getPlcNoAnn)
import PlutusTx.Prelude (Integer, ($), (.), foldr, (*), BuiltinByteString, popCountByteString, (==), otherwise)
import PlutusTx.Numeric ((+), zero, one)

import PlutusCore (DefaultFun, DefaultUni)
import UntypedPlutusCore qualified as UPLC

import Prelude qualified as Haskell
import Hedgehog.Internal.Gen qualified as G
import Hedgehog.Internal.Range qualified as R
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString (ByteString)
import PlutusTx.Builtins ( testBitByteString, shiftByteString )
import Plutus.Crypto.BlsField (Scalar)

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