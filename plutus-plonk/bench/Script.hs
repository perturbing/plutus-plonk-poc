{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

module Script where

import PlutusTx.Prelude ( Integer, Bool (..))
import PlutusTx (compile, CompiledCode)

import Plutus.Crypto.Plonk.Inputs ( PreInputsFast, ProofFast )
import Plutus.Crypto.Plonk.Verifier ( verifyPlonkFast )

verifyPlonkCode :: CompiledCode (PreInputsFast -> [Integer] -> ProofFast -> Bool)
verifyPlonkCode = $$(compile [|| verifyPlonkFast ||])