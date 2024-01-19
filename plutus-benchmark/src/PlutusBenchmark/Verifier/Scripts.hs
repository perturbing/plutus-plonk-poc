{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module PlutusBenchmark.Verifier.Scripts
( verifyPlonkFastScript
, verifyPlonkScript
) where

import PlutusTx (compile, getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import PlutusTx.Prelude (Integer, ($))

import PlutusCore (DefaultFun, DefaultUni)
import UntypedPlutusCore qualified as UPLC

import Plutus.Crypto.Plonk (PreInputs, PreInputsFast, Proof, ProofFast, verifyPlonk,
                            verifyPlonkFast)

verifyPlonkFastScript :: PreInputsFast -> [Integer] -> ProofFast -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
verifyPlonkFastScript preIn pub proof =
    getPlcNoAnn $ $$(compile [|| verifyPlonkFast ||])
       `unsafeApplyCode` liftCodeDef preIn
       `unsafeApplyCode` liftCodeDef pub
       `unsafeApplyCode` liftCodeDef proof

verifyPlonkScript :: PreInputs -> [Integer] -> Proof -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
verifyPlonkScript preIn pub proof =
    getPlcNoAnn $ $$(compile [|| verifyPlonk ||])
       `unsafeApplyCode` liftCodeDef preIn
       `unsafeApplyCode` liftCodeDef pub
       `unsafeApplyCode` liftCodeDef proof
