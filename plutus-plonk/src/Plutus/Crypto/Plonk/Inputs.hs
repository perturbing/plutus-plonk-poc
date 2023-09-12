{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Plutus.Crypto.Plonk.Inputs 
(
) where

import Plutus.Crypto.Plonk.Polynomial (Polynomial)
import PlutusTx.Builtins (BuiltinByteString, Integer, BuiltinBLS12_381_G1_Element)
import PlutusTx ( unstableMakeIsData )
import qualified Prelude as Haskell

-- Proof is a type that wraps all necesary elements needed for a proof.
-- Note that the G1 elements are compressed as bytestrings.
data Proof = Proof {
    commitment_a     :: BuiltinByteString, -- G1 element
    commitment_b     :: BuiltinByteString, -- G1 element
    commitment_c     :: BuiltinByteString, -- G1 element
    commitment_z     :: BuiltinByteString, -- G1 element
    t_low            :: BuiltinByteString, -- G1 element
    t_mid            :: BuiltinByteString, -- G1 element
    t_high           :: BuiltinByteString, -- G1 element
    w_omega          :: BuiltinByteString, -- G1 element
    w_omega_zeta     :: BuiltinByteString, -- G1 element
    a_eval           :: Integer,           -- Field element
    b_eval           :: Integer,           -- Field element
    c_eval           :: Integer,           -- Field element
    s_sig1           :: Integer,           -- Field element
    s_sig2           :: Integer,           -- Field element
    z_omega          :: Integer            -- Field element
} deriving (Haskell.Show)
unstableMakeIsData ''Proof

-- PreInputs are the know points, polynomials needed for plonk verifier.
-- These are known before proof construction.
data PreInputs = PreInputs {
    n               :: Integer,                     -- number of constraints in the circuit
    lagrangeBase    :: [Polynomial],                -- The lagrange base used
    qM              :: BuiltinBLS12_381_G1_Element, 
    qL              :: BuiltinBLS12_381_G1_Element,
    qR              :: BuiltinBLS12_381_G1_Element,
    qO              :: BuiltinBLS12_381_G1_Element,
    qC              :: BuiltinBLS12_381_G1_Element,
    sSig1           :: BuiltinBLS12_381_G1_Element,
    sSig2           :: BuiltinBLS12_381_G1_Element,
    sSig3           :: BuiltinBLS12_381_G1_Element
} deriving (Haskell.Show)
unstableMakeIsData ''PreInputs
