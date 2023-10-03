{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Plutus.Crypto.Plonk.Verifier
( verifyPlonk
, verifyPlonkFast
) where

import Plutus.Crypto.Plonk.Inputs (PreInputs (..), Proof (..), PreInputsFast (..), ProofFast (..))
import Plutus.Crypto.BlsField (mkScalar, Scalar (unScalar), MultiplicativeGroup (..))
import Plutus.Crypto.Plonk.Transcript (challengeScalar, transcriptPoint, transcriptScalar, transcriptNew, getTranscript)
import Plutus.Crypto.Number.ModArithmetic (exponentiate)

import PlutusTx.Prelude (Integer, Bool (..), bls12_381_G1_uncompress, bls12_381_G1_scalarMul, bls12_381_G1_generator
                        ,BuiltinBLS12_381_G1_Element, sum, BuiltinBLS12_381_G2_Element, bls12_381_finalVerify
                        ,bls12_381_G2_generator, bls12_381_millerLoop, (>), otherwise, enumFromTo, (.), (&&))
import PlutusTx.Eq (Eq (..))
import PlutusTx.List (map, zipWith, foldr, head, and)
import PlutusTx.ErrorCodes (predOrderingBadArgumentError)
import PlutusTx.Numeric
    ( AdditiveGroup(..),
      AdditiveMonoid(..),
      AdditiveSemigroup(..),
      Module(..),
      MultiplicativeMonoid(one),
      MultiplicativeSemigroup((*)),
      negate )
import PlutusTx.Builtins (bls12_381_G1_add, bls12_381_G1_zero, bls12_381_G1_neg, bls12_381_G2_add
                         ,bls12_381_G2_zero, bls12_381_G2_neg, bls12_381_G2_scalarMul)

-- a general vanilla plonk verifier. 
-- Note that the viewpattern match in the inputs of this function
-- is to make sure that only correctly formatted inputs are provided.
-- That is, all provided inputs have to be parsed, upon
-- incorrect parsing, the script will fail.
{-# INLINEABLE verifyPlonk #-}
verifyPlonk :: PreInputs -> [Integer] -> Proof -> Bool
verifyPlonk preInputs@(PreInputs nPub p k1 k2 qM qL qR qO qC sSig1 sSig2 sSig3 x2 gen)
            pubInputs
            proof@(Proof ca cb cc cz ctl ctm cth cwo cwz ea eb ec es1 es2 ez)
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
    , let (w1 : wxs) = map (negate . mkScalar) pubInputs
    =
        -- this step could be done offchain?
    let n = exponentiate 2 p
        -- get the transcript variables
        (beta, gamma, alpha, zeta, v, u) = getTranscript commA commB commC commZ commTLow commTMid commTHigh evalA evalB evalC evalS1 evalS2 evalZOmega commWOmega commWOmegaZeta
        -- this is Z_H(zeta) in the plonk paper
        zeroPoly = scale n zeta - one
        -- this is L_1(zeta) and the higher order L_i 
        (lagrangePoly1 : lagrangePolyXs) = map (\i -> (scale i gen * zeroPoly) * recip (mkScalar n * (zeta - scale i gen))) (enumFromTo 1 nPub)
        -- this is PI(zeta) in the plonk paper
        piZeta = w1 * lagrangePoly1 + sum (zipWith (*) wxs lagrangePolyXs)
        -- this is r_0 in the plonk paper
        r0 = piZeta - lagrangePoly1 * alpha * alpha - alpha * (evalA + beta*evalS1 + gamma) * (evalB + beta*evalS2 + gamma) * (evalC + gamma) * evalZOmega
        -- this is [D]_1 in the plonk paper
        batchPolyCommitG1 = scale (evalA*evalB) qM
                          + scale evalA qL
                          + scale evalB qR
                          + scale evalC qO
                          + qC
                          + scale ((evalA + beta * zeta + gamma)*(evalB +beta*k1*zeta + gamma)*(evalC + beta*k2*zeta + gamma)*alpha + lagrangePoly1*alpha * alpha + u) commZ
                          - scale ((evalA +beta*evalS1+gamma)*(evalB + beta*evalS2+gamma)*alpha*beta*evalZOmega) sSig3
                          - scale zeroPoly (commTLow + scale (scale n zeta) commTMid + scale (scale (2*n) zeta) commTHigh)
        -- this is [F]_1 in the plonk paper
        batchPolyCommitFull = batchPolyCommitG1 + scale v (commA + scale v (commB + scale v (commC + scale v (sSig1 + scale v sSig2))))
        -- this is [E]_1 in the plonk paper
        groupEncodedBatchEval = scale (negate r0 + v * (evalA + v * (evalB + v * (evalC + v * (evalS1 + v * evalS2)))) + u*evalZOmega ) bls12_381_G1_generator
    in
    -- the final check that under the pairing.
    bls12_381_finalVerify (bls12_381_millerLoop (commWOmega + scale u commWOmegaZeta) x2) (bls12_381_millerLoop (scale zeta commWOmega + scale (u*zeta*gen) commWOmegaZeta + batchPolyCommitFull - groupEncodedBatchEval) bls12_381_G2_generator)

-- a general vanilla plonk verifier optimised. 
{-# INLINEABLE verifyPlonkFast #-}
verifyPlonkFast :: PreInputsFast -> [Integer] -> ProofFast -> Bool
verifyPlonkFast preInputsFast@(PreInputsFast n k1 k2 qM qL qR qO qC sSig1 sSig2 sSig3 x2 gens)
            pubInputs
            proofFast@(ProofFast ca cb cc cz ctl ctm cth cwo cwz ea eb ec es1 es2 ez lagInv)
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
    , let (w1 : wxs) = map (negate . mkScalar) pubInputs
    , let lagsInv = map mkScalar lagInv
    = let (beta, transcript1) = challengeScalar (transcriptPoint (transcriptPoint (transcriptPoint (transcriptNew "testing the prover") "commitment a" commA) "commitment b" commB) "commitment c" commC) "beta"
          (gamma, transcript2) = challengeScalar transcript1 "gamma"
          (alpha,transcript3) = challengeScalar (transcriptPoint transcript2 "Permutation polynomial" commZ) "alpha"
          (zeta, transcript4) = challengeScalar (transcriptPoint (transcriptPoint (transcriptPoint transcript3 "Quotient low polynomial" commTLow) "Quotient mid polynomial" commTMid) "Quotient high polynomial" commTHigh) "zeta"
          (v, transcript5) = challengeScalar (transcriptScalar (transcriptScalar (transcriptScalar (transcriptScalar (transcriptScalar (transcriptScalar transcript4 "Append a_eval." evalA) "Append b_eval." evalB) "Append c_eval." evalC) "Append s_sig1." evalS1) "Append s_sig2." evalS2) "Append z_omega." evalZOmega) "v"
          (u, _) = challengeScalar (transcriptPoint (transcriptPoint transcript5 "w_omega comm" commWOmega) "w_omega_zeta comm" commWOmegaZeta) "u"
          (lagrangePoly1 : lagrangePolyXs) = zipWith (\x y -> x * (scale n zeta - one) * y) gens lagsInv
          piZeta = w1 * lagrangePoly1 + sum (zipWith (*) wxs lagrangePolyXs)
          r0 = piZeta - lagrangePoly1*alpha*alpha - alpha*(evalA + beta*evalS1 + gamma)*(evalB + beta*evalS2 + gamma)*(evalC + gamma)*evalZOmega
          batchPolyCommitG1 = scale (evalA*evalB) qM
                            + scale evalA qL
                            + scale evalB qR
                            + scale evalC qO
                            + qC
                            + scale ((evalA + beta*zeta + gamma)*(evalB +beta*k1*zeta + gamma)*(evalC + beta*k2*zeta + gamma)*alpha + lagrangePoly1*alpha*alpha + u) commZ
                            - scale ((evalA +beta*evalS1+gamma)*(evalB + beta*evalS2 + gamma)*alpha*beta*evalZOmega) sSig3
                            - scale (scale n zeta - one) (commTLow + scale (scale n zeta) commTMid + scale (scale (2*n) zeta) commTHigh)
          batchPolyCommitFull = batchPolyCommitG1 + scale v (commA + scale v (commB + scale v (commC + scale v (sSig1 + scale v sSig2))))
          groupEncodedBatchEval = scale (negate r0 + v * (evalA + v * (evalB + v * (evalC + v * (evalS1 + v * evalS2)))) + u*evalZOmega ) bls12_381_G1_generator
    in bls12_381_finalVerify (bls12_381_millerLoop (commWOmega + scale u commWOmegaZeta) x2) (bls12_381_millerLoop (scale zeta commWOmega + scale (u*zeta*head gens) commWOmegaZeta + batchPolyCommitFull - groupEncodedBatchEval) bls12_381_G2_generator)
       && and (zipWith (\x y -> x * mkScalar n * (zeta - y) == one) lagsInv gens) -- this accounts for some 5% of the calc