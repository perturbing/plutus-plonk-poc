{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Plutus.Crypto.Plonk.Inputs
( Proof (..)
, PreInputs (..)
, ProofFast (..)
, PreInputsFast (..)
, convertToFastProof
, convertToFastPreInputs
) where

import Plutus.Crypto.BlsField (Scalar, mkScalar, recip, unScalar)
import Plutus.Crypto.Plonk.Transcript (getTranscript)
import PlutusTx (makeIsDataIndexed, makeLift, unstableMakeIsData)
import PlutusTx.Builtins (BuiltinBLS12_381_G1_Element, BuiltinBLS12_381_G2_Element,
                          BuiltinByteString, Integer)
import PlutusTx.Numeric (AdditiveGroup (..), scale, (*))
import PlutusTx.Prelude (bls12_381_G1_uncompress, enumFromTo, map, ($), (.))

import Prelude qualified as Haskell

-- Proof is a type that wraps all necesary elements needed for a proof.
-- Note that the G1 elements are compressed as bytestrings.
-- The field elements are represented as integers as they are provided
-- by the prover and need to be checked to be in the field.
data Proof = Proof
    { commitmentA :: BuiltinByteString -- a serialized G1 element
    , commitmentB :: BuiltinByteString -- a serialized G1 element
    , commitmentC :: BuiltinByteString -- a serialized G1 element
    , commitmentZ :: BuiltinByteString -- a serialized G1 element
    , tLow        :: BuiltinByteString -- a serialized G1 element
    , tMid        :: BuiltinByteString -- a serialized G1 element
    , tHigh       :: BuiltinByteString -- a serialized G1 element
    , wOmega      :: BuiltinByteString -- a serialized G1 element
    , wOmegaZeta  :: BuiltinByteString -- a serialized G1 element
    , aEval       :: Integer           -- Field element
    , bEval       :: Integer           -- Field element
    , cEval       :: Integer           -- Field element
    , sSig1P      :: Integer           -- Field element
    , sSig2P      :: Integer           -- Field element
    , zOmega      :: Integer           -- Field element
    } deriving (Haskell.Show)

makeLift ''Proof
makeIsDataIndexed ''Proof [('Proof,0)]

-- PreInputs are the minimal values that parametrize
-- a plonk verifier. These values are known before proof
-- generation.
data PreInputs = PreInputs
    { nPublic   :: Integer                     -- number of public inputs
    , power     :: Integer                     -- power, 2^power >= number of constraints in the circuit (the upper bound used)
    , k1        :: Scalar                      -- The first field elements that creates a disjoint left coset of H
    , k2        :: Scalar                      -- The second field element that creates a disjoint left coset of H
    , qM        :: BuiltinByteString -- the commited polynomial of the multiplication gates
    , qL        :: BuiltinByteString -- the commited polynomial of the left inputs of the circuit
    , qR        :: BuiltinByteString -- the commited polynomial of the right inputs of the circuits
    , qO        :: BuiltinByteString -- the commited polynomial of the outputs of the circuit
    , qC        :: BuiltinByteString -- the commited polynomial of the constants inputs of the circuit
    , sSig1     :: BuiltinByteString -- the commited polynomial of the first wire permutation
    , sSig2     :: BuiltinByteString -- the commited polynomial of the second wire permutation
    , sSig3     :: BuiltinByteString -- the commited polynomial of the third wire permutation
    , x2        :: BuiltinByteString -- the first order of the SRS over G2 (g^x where x is the toxic waste / tau in power of tau)
    , generator :: Scalar                      -- generator of the subgroup H
    } deriving (Haskell.Show)

makeLift ''PreInputs
makeIsDataIndexed ''PreInputs [('PreInputs,0)]

-- PreInputsFast are the optimised minimal values that parametrize
-- a plonk verifier. These values are known before proof
-- generation.
data PreInputsFast = PreInputsFast
    { n'         :: Integer                     -- n number of constraints (upper bound) used
    , pow'       :: Integer                     -- n = 2^pow
    , k1'        :: Scalar                      -- The first field elements that creates a disjoint left coset of H
    , k2'        :: Scalar                      -- The second field element that creates a disjoint left coset of H
    , qM'        :: BuiltinByteString -- the commited polynomial of the multiplication gates
    , qL'        :: BuiltinByteString -- the commited polynomial of the left inputs of the circuit
    , qR'        :: BuiltinByteString -- the commited polynomial of the right inputs of the circuits
    , qO'        :: BuiltinByteString -- the commited polynomial of the outputs of the circuit
    , qC'        :: BuiltinByteString -- the commited polynomial of the constants inputs of the circuit
    , sSig1'     :: BuiltinByteString -- the commited polynomial of the first wire permutation
    , sSig2'     :: BuiltinByteString -- the commited polynomial of the second wire permutation
    , sSig3'     :: BuiltinByteString -- the commited polynomial of the third wire permutation
    , x2'        :: BuiltinByteString -- the first order of the SRS over G2 (g^x where x is the toxic waste / tau in power of tau)
    , generators :: [Scalar]                    -- powers of the generator of the subgroup H, [g,g^2,..,g^{number of public inputs}]
    } deriving (Haskell.Show)

makeLift ''PreInputsFast
makeIsDataIndexed ''PreInputsFast [('PreInputsFast,0)]

-- Proof is a type that wraps all necesary elements needed for a proof.
-- Note that the G1 elements are compressed as bytestrings.
-- The field elements are represented as integers as they are provided
-- by the prover and need to be checked to be in the field.
data ProofFast = ProofFast
    { commitmentA'     :: BuiltinByteString -- a serialized G1 element
    , commitmentB'     :: BuiltinByteString -- a serialized G1 element
    , commitmentC'     :: BuiltinByteString -- a serialized G1 element
    , commitmentZ'     :: BuiltinByteString -- a serialized G1 element
    , tLow'            :: BuiltinByteString -- a serialized G1 element
    , tMid'            :: BuiltinByteString -- a serialized G1 element
    , tHigh'           :: BuiltinByteString -- a serialized G1 element
    , wOmega'          :: BuiltinByteString -- a serialized G1 element
    , wOmegaZeta'      :: BuiltinByteString -- a serialized G1 element
    , aEval'           :: Integer           -- Field element
    , bEval'           :: Integer           -- Field element
    , cEval'           :: Integer           -- Field element
    , sSig1P'          :: Integer           -- Field element
    , sSig2P'          :: Integer           -- Field element
    , zOmega'          :: Integer           -- Field element
    , lagrangeInverses :: [Integer]         -- A list of inversed of step 6, to save onchain calc.
    } deriving (Haskell.Show)

makeLift ''ProofFast
makeIsDataIndexed ''ProofFast [('ProofFast,0)]

convertToFastPreInputs :: PreInputs -> PreInputsFast
convertToFastPreInputs preInputs@(PreInputs nPub p k1 k2 qM qL qR qO qC sSig1 sSig2 sSig3 x2 gen )
    = PreInputsFast
    { n'            = 2 Haskell.^ p
    , pow'          = p
    , k1'           = k1
    , k2'           = k2
    , qM'           = qM
    , qL'           = qL
    , qR'           = qR
    , qO'           = qO
    , qC'           = qC
    , sSig1'        = sSig1
    , sSig2'        = sSig2
    , sSig3'        = sSig3
    , x2'           = x2
    , generators    = map (`scale` gen) (enumFromTo 1 nPub)
    }

convertToFastProof :: PreInputsFast -> Proof -> ProofFast
convertToFastProof preInputsFast proof@(Proof ca cb cc cz ctl ctm cth cwo cwz ea eb ec es1 es2 ez)
    = ProofFast
    { commitmentA' = ca
    , commitmentB' = cb
    , commitmentC' = cc
    , commitmentZ' = cz
    , tLow'        = ctl
    , tMid'        = ctm
    , tHigh'       = cth
    , wOmega'      = cwo
    , wOmegaZeta'  = cwz
    , aEval'       = ea
    , bEval'       = eb
    , cEval'       = ec
    , sSig1P'      = es1
    , sSig2P'      = es2
    , zOmega'      = ez
    , lagrangeInverses = let (_, _, _, zeta, _, _) = getTranscript
                                    (bls12_381_G1_uncompress ca)
                                    (bls12_381_G1_uncompress cb)
                                    (bls12_381_G1_uncompress cc)
                                    (bls12_381_G1_uncompress cz)
                                    (bls12_381_G1_uncompress ctl)
                                    (bls12_381_G1_uncompress ctm)
                                    (bls12_381_G1_uncompress cth)
                                    (mkScalar ea)
                                    (mkScalar eb)
                                    (mkScalar ec)
                                    (mkScalar es1)
                                    (mkScalar es2)
                                    (mkScalar ez)
                                    (bls12_381_G1_uncompress cwo)
                                    (bls12_381_G1_uncompress cwz)
                         in map (unScalar . recip . (\x -> mkScalar (n' preInputsFast) * (zeta - x))) $ generators preInputsFast
    }
