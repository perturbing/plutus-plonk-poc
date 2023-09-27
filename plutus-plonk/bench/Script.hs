{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Script where

import PlutusTx.Prelude ( Integer, Bool (..))
import PlutusTx (compile, CompiledCode)

import Plutus.Crypto.Plonk.Inputs ( PreInputs, Proof )
import Plutus.Crypto.Plonk.Verifier ( verifyPlonk )
import PlutusLedgerApi.V3 (serialiseCompiledCode, SerialisedScript)

verifyPlonkCode :: CompiledCode (PreInputs -> [Integer] -> Proof -> Bool)
verifyPlonkCode = $$(compile [|| verifyPlonk ||])

plonkPlutus :: SerialisedScript
plonkPlutus = serialiseCompiledCode verifyPlonkCode
