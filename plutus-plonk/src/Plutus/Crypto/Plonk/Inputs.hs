{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Plutus.Crypto.Plonk.Inputs 
( Proof (..)
, PreInputs (..)
) where

import Plutus.Crypto.BlsField (Scalar)
import PlutusTx.Builtins (BuiltinByteString, Integer, BuiltinBLS12_381_G1_Element, BuiltinBLS12_381_G2_Element)
import PlutusTx (makeLift, makeIsDataIndexed, unstableMakeIsData)
import qualified Prelude as Haskell

-- Proof is a type that wraps all necesary elements needed for a proof.
-- Note that the G1 elements are compressed as bytestrings.
-- The field elements are represented as integers as they are provided 
-- by the prover and need to be checked to be in the field.
data Proof = Proof 
    { commitmentA     :: BuiltinByteString -- a serialized G1 element
    , commitmentB     :: BuiltinByteString -- a serialized G1 element
    , commitmentC     :: BuiltinByteString -- a serialized G1 element
    , commitmentZ     :: BuiltinByteString -- a serialized G1 element
    , tLow            :: BuiltinByteString -- a serialized G1 element
    , tMid            :: BuiltinByteString -- a serialized G1 element
    , tHigh           :: BuiltinByteString -- a serialized G1 element
    , wOmega          :: BuiltinByteString -- a serialized G1 element
    , wOmegaZeta      :: BuiltinByteString -- a serialized G1 element
    , aEval           :: Integer           -- Field element
    , bEval           :: Integer           -- Field element
    , cEval           :: Integer           -- Field element
    , sSig1P          :: Integer           -- Field element
    , sSig2P          :: Integer           -- Field element
    , zOmega          :: Integer           -- Field element
    } deriving (Haskell.Show)

makeLift ''Proof
makeIsDataIndexed ''Proof [('Proof,0)]

-- PreInputs are the minimal values that parametrize
-- a plonk verifier. These values are known before proof
-- generation.
data PreInputs = PreInputs 
    { nPublic         :: Integer                     -- number of public inputs
    , power           :: Integer                     -- power, 2^power >= number of constraints in the circuit (the upper bound used)
    , k1              :: Scalar                      -- The first field elements that creates a disjoint left coset of H
    , k2              :: Scalar                      -- The second field element that creates a disjoint left coset of H 
    , qM              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the multiplication gates 
    , qL              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the left inputs of the circuit
    , qR              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the right inputs of the circuits
    , qO              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the outputs of the circuit
    , qC              :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the constants inputs of the circuit
    , sSig1           :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the first wire permutation 
    , sSig2           :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the second wire permutation
    , sSig3           :: BuiltinBLS12_381_G1_Element -- the commited polynomial of the third wire permutation
    , x2              :: BuiltinBLS12_381_G2_Element -- the first order of the SRS over G2 (g^x where x is the toxic waste / tau in power of tau)
    , generator       :: Scalar                       -- generator of the subgroup H
    } deriving (Haskell.Show)

makeLift ''PreInputs
makeIsDataIndexed ''PreInputs [('PreInputs,0)]