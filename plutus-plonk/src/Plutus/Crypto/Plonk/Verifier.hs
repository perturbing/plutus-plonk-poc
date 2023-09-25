{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

module Plutus.Crypto.Plonk.Verifier
( verifyPlonk
) where

import Plutus.Crypto.Plonk.Inputs (PreInputs (..), Proof (..))
import Plutus.Crypto.BlsField (mkScalar, Scalar (unScalar), MultiplicativeGroup (..))
import Plutus.Crypto.Plonk.Transcript (challengeScalar, transcriptPoint, transcriptScalar, transcriptNew, getTranscript)
import Plutus.Crypto.Number.ModArithmetic (exponentiate)

import PlutusTx.Prelude (Integer, Bool (..), bls12_381_G1_uncompress, bls12_381_G1_scalarMul, bls12_381_G1_generator, BuiltinBLS12_381_G1_Element, sum)
import PlutusTx.Numeric (one, zero, scale, (*), (+), AdditiveGroup (..), negate)
import PlutusTx.Eq (Eq (..))
import PlutusTx.List (map, zipWith)
import PlutusTx.ErrorCodes (predOrderingBadArgumentError)

-- a general vanilla plonk verifier. 
-- Note that the viewpattern match in the inputs of this function
-- is to make sure that only correctly formatted inputs are provided.
-- That is, all provided inputs have to be parsed, upon
-- incorrect parsing, the script will fail.
{-# INLINEABLE verifyPlonk #-}
verifyPlonk :: PreInputs -> [Integer] -> Proof -> Scalar --Bool
verifyPlonk preInputs pubInputs proof@(Proof ca cb cc cz ctl ctm cth cwo cwz ea eb ec es1 es2 ez)
    | (bls12_381_G1_uncompress -> commA) <- ca
    , (bls12_381_G1_uncompress -> commB) <- cb
    , (bls12_381_G1_uncompress -> commC) <- cc
    , (bls12_381_G1_uncompress -> commZ) <- cz
    , (bls12_381_G1_uncompress -> commTLow) <- ctl
    , (bls12_381_G1_uncompress -> commTMid) <- ctm
    , (bls12_381_G1_uncompress -> commTHigh) <- cth
    , (bls12_381_G1_uncompress -> commWOmega) <- cwo
    , (bls12_381_G1_uncompress -> commWOmegaZeta) <- cwz
    , (mkScalar -> evalA) <- ea
    , (mkScalar -> evalB) <- eb
    , (mkScalar -> evalC) <- ec
    , (mkScalar -> evalS1) <- es1
    , (mkScalar -> evalS2) <- es2
    , (mkScalar -> evalZOmega) <- ez
    , let (w1 : wxs) = map mkScalar pubInputs
    =
        -- this step could be done offchain?
    let n = exponentiate 2 (power preInputs) 
        -- this is the generator of H (\omega in the paper)
        w = generator preInputs 
        -- get the transcript variables
        (beta, gamma, alpha, zeta, v, u) = getTranscript commA commB commC commZ commTLow commTMid commTHigh evalA evalB evalC evalS1 evalS2 evalZOmega commWOmega commWOmegaZeta
        -- this is Z_H(zeta) in the plonk paper
        zeroPoly = scale n zeta - one
        -- this is L_1(zeta) and the higher order L_i 
        (lagrangePoly1 : lagrangePolyXs) = map (\i -> (scale i w * zeroPoly) * recip (mkScalar n * (zeta - scale i w))) [1.. nPublic preInputs ]
        -- this is PI(zeta) in the plonk paper
        piZeta = w1 * lagrangePoly1 + sum (zipWith (*) wxs lagrangePolyXs) 
        -- this is r_0 in the plonk paper
        r0 = negate piZeta - lagrangePoly1 * alpha * alpha - alpha * (evalA + beta*evalS1 + gamma) * (evalB + beta*evalS2 + gamma) * (evalC + gamma) * evalZOmega
    in
    r0

    -- Also todo:
    -- write interface for reading snarkjs outputs (preinputs/proof/public inputs)
    -- extract from snarkjs how they do their transcript
    -- make a full dapp

    -- random remarks: the thing with deconding the snark js verification key is
    -- that the G2 point is in some montgomery form. TODO: find the serialisation
    -- of that point