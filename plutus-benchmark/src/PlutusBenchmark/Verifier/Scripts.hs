{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module PlutusBenchmark.Verifier.Scripts 
( verifyPlonkFastScript
, verifyPlonkScript
) where

import PlutusTx
    ( getPlcNoAnn, unsafeApplyCode, liftCodeDef, compile ) 
import PlutusTx.Prelude ( Integer, ($) )

import PlutusCore (DefaultFun, DefaultUni)
import UntypedPlutusCore qualified as UPLC

import Plutus.Crypto.Plonk (verifyPlonkFast, PreInputsFast, ProofFast, Proof, PreInputs, verifyPlonk)

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