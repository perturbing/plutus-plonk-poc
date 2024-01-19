{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Script where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude (Bool (..), Integer)

import Plutus.Crypto.Plonk (PreInputsFast, ProofFast, verifyPlonkFast)

verifyPlonkCode :: CompiledCode (PreInputsFast -> [Integer] -> ProofFast -> Bool)
verifyPlonkCode = $$(compile [|| verifyPlonkFast ||])
