{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Plutus.Crypto.Plonk.Inputs 
( Proof (..)
, PreInputs (..)
) where

import Plutus.Crypto.Plonk.Polynomial (Polynomial)
import Plutus.Crypto.BlsField ( Scalar )
import PlutusTx.Builtins (BuiltinByteString, Integer, BuiltinBLS12_381_G1_Element, BuiltinBLS12_381_G2_Element)
import PlutusTx ( unstableMakeIsData )
import qualified Prelude as Haskell

-- Proof is a type that wraps all necesary elements needed for a proof.
-- Note that the G1 elements are compressed as bytestrings.
-- The field elements are represented as integers as they are provided 
-- by the prover and need to be checked to be in the field.
data Proof = Proof {
    commitment_a     :: BuiltinByteString, -- a serialized G1 element
    commitment_b     :: BuiltinByteString, -- a serialized G1 element
    commitment_c     :: BuiltinByteString, -- a serialized G1 element
    commitment_z     :: BuiltinByteString, -- a serialized G1 element
    t_low            :: BuiltinByteString, -- a serialized G1 element
    t_mid            :: BuiltinByteString, -- a serialized G1 element
    t_high           :: BuiltinByteString, -- a serialized G1 element
    w_omega          :: BuiltinByteString, -- a serialized G1 element
    w_omega_zeta     :: BuiltinByteString, -- a serialized G1 element
    a_eval           :: Integer,           -- Field element
    b_eval           :: Integer,           -- Field element
    c_eval           :: Integer,           -- Field element
    s_sig1           :: Integer,           -- Field element
    s_sig2           :: Integer,           -- Field element
    z_omega          :: Integer            -- Field element
} deriving (Haskell.Show)
unstableMakeIsData ''Proof

-- PreInputs are the known points, polynomials needed for plonk verifier.
data PreInputs = PreInputs {
    power           :: Integer,                     -- power, 2^power >= number of constraints in the circuit (the upper bound used)
    l               :: Integer,                     -- number of public inputs
    lagrangeBase    :: [Polynomial],                -- The lagrange base used
    w               :: Scalar,                      -- generator of the subgroup H
    k1              :: Scalar,                      -- The first field elements that creates a disjoint left coset of H
    k2              :: Scalar,                      -- The second field element that creates a disjoint left coset of H 
    qM              :: BuiltinBLS12_381_G1_Element, -- the commited polynomial of the multiplication gates 
    qL              :: BuiltinBLS12_381_G1_Element, -- the commited polynomial of the left inputs of the circuit
    qR              :: BuiltinBLS12_381_G1_Element, -- the commited polynomial of the right inputs of the circuits
    qO              :: BuiltinBLS12_381_G1_Element, -- the commited polynomial of the outputs of the circuit
    qC              :: BuiltinBLS12_381_G1_Element, -- the commited polynomial of the constants inputs of the circuit
    sSig1           :: BuiltinBLS12_381_G1_Element, -- the commited polynomial of the first wire permutation 
    sSig2           :: BuiltinBLS12_381_G1_Element, -- the commited polynomial of the second wire permutation
    sSig3           :: BuiltinBLS12_381_G1_Element,  -- the commited polynomial of the third wire permutation
    x               :: BuiltinBLS12_381_G2_Element  -- the first order of the SRS (g^x where x is the toxic waste)
} deriving (Haskell.Show)
unstableMakeIsData ''PreInputs
