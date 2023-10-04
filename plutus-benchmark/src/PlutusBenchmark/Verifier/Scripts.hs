{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}

module PlutusBenchmark.Verifier.Scripts 
( verifyPlonkFastScript
) where

import PlutusTx 
import PlutusTx.Prelude

import PlutusCore (DefaultFun, DefaultUni)
import UntypedPlutusCore qualified as UPLC

import Plutus.Crypto.Plonk (verifyPlonkFast, PreInputsFast, ProofFast)

import Prelude qualified as Haskell

verifyPlonkFastScript :: PreInputsFast -> [Integer] -> ProofFast -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
verifyPlonkFastScript preIn pub proof = 
    getPlcNoAnn $ $$(compile [|| verifyPlonkFast ||]) 
       `unsafeApplyCode` liftCodeDef preIn
       `unsafeApplyCode` liftCodeDef pub
       `unsafeApplyCode` liftCodeDef proof
