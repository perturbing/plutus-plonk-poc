{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE Strict             #-}
{-# LANGUAGE ViewPatterns       #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Plutus.Crypto.Plonk.Verifier
( verifyPlonk
, verifyPlonkFast
) where

import Plutus.Crypto.Plonk.Inputs (PreInputs (..), Proof (..), PreInputsFast (..), ProofFast (..))
import Plutus.Crypto.BlsField (mkScalar, Scalar (..), MultiplicativeGroup (..), powerOfTwoExponentiation)
import Plutus.Crypto.Plonk.Transcript (challengeScalar, transcriptPoint, transcriptScalar, transcriptNew, getTranscript)
import PlutusTx.Prelude (Integer, Bool (..), bls12_381_G1_uncompress, bls12_381_G1_scalarMul, bls12_381_G1_generator
                        ,BuiltinBLS12_381_G1_Element, sum, BuiltinBLS12_381_G2_Element, bls12_381_finalVerify
                        ,bls12_381_G2_generator, bls12_381_millerLoop, (>), otherwise, enumFromTo, (.), (&&), divide, error, (<), (||), even, (<>), takeByteString, ($), integerToByteString, bls12_381_G1_compress)
import PlutusTx.Eq (Eq (..))
import PlutusTx.List (map, zipWith, foldr, head, and)
import PlutusTx.Numeric
    ( AdditiveGroup(..),
      AdditiveMonoid(..),
      AdditiveSemigroup(..),
      Module(..),
      MultiplicativeMonoid(one),
      MultiplicativeSemigroup((*)),
      negate )
import PlutusTx.Builtins (blake2b_256, byteStringToInteger)
import PlutusCore (DefaultFun(IntegerToByteString))

{-# INLINABLE exponentiate #-}
exponentiate :: Integer -> Integer -> Integer
exponentiate x n
    | n < 0 || x < 0    = error ()
    | n == 0            = 1
    | x == 0            = 0
    | even n            = exponentiate x (n `divide` 2) * exponentiate x (n `divide` 2)
    | otherwise         = x * exponentiate x ((n - 1) `divide` 2) * exponentiate x ((n - 1) `divide` 2)

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
    | (mkScalar -> evalA) <- ea
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
        (beta, gamma, alpha, zeta, v, u) = getTranscript ca cb cc cz ctl ctm cth evalA evalB evalC evalS1 evalS2 evalZOmega cwo cwz
        -- this is Z_H(zeta) in the plonk paper
        zetaN = scale n zeta
        zeroPoly = zetaN - one
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
                          + scale ((evalA + beta * zeta + gamma)*(evalB +beta*k1*zeta + gamma)*(evalC + beta*k2*zeta + gamma)*alpha + lagrangePoly1*alpha * alpha + u) cz
                          - scale ((evalA +beta*evalS1+gamma)*(evalB + beta*evalS2+gamma)*alpha*beta*evalZOmega) sSig3
                          - scale zeroPoly (ctl + scale zetaN ctm + scale (scale 2 zetaN) cth)
        -- this is [F]_1 in the plonk paper
        batchPolyCommitFull = batchPolyCommitG1 + scale v (ca + scale v (cb + scale v (cc + scale v (sSig1 + scale v sSig2))))
        -- this is [E]_1 in the plonk paper
        groupEncodedBatchEval = scale (negate r0 + v * (evalA + v * (evalB + v * (evalC + v * (evalS1 + v * evalS2)))) + u*evalZOmega ) bls12_381_G1_generator
    in 
    -- the final check that under the pairing.
    bls12_381_finalVerify (bls12_381_millerLoop (cwo + scale u cwz) x2) (bls12_381_millerLoop (scale zeta cwo + scale (u*zeta*gen) cwz + batchPolyCommitFull - groupEncodedBatchEval) bls12_381_G2_generator)

-- a general vanilla plonk verifier optimised. 
{-# INLINEABLE verifyPlonkFast #-}
verifyPlonkFast :: PreInputsFast -> [Integer] -> ProofFast -> Bool
verifyPlonkFast preInputsFast@(PreInputsFast n p k1 k2 qM qL qR qO qC sSig1 sSig2 sSig3 x2 gens)
            pubInputs
            proofFast@(ProofFast ca cb cc cz ctl ctm cth cwo cwz ea eb ec es1 es2 ez lagInv)
    | (mkScalar -> evalA) <- ea
    , (mkScalar -> evalB) <- eb
    , (mkScalar -> evalC) <- ec
    , (mkScalar -> evalS1) <- es1
    , (mkScalar -> evalS2) <- es2
    , (mkScalar -> evalZOmega) <- ez
    , let (w1 : wxs) = map (negate . mkScalar) pubInputs
    , let lagsInv = map mkScalar lagInv
    = let transcript0 = "FS transcriptdom-septesting the provercommitment a" <> bls12_381_G1_compress ca <> "commitment b" 
                                                                             <> bls12_381_G1_compress cb <> "commitment c" 
                                                                             <> bls12_381_G1_compress cc <> "beta"
          beta = Scalar . byteStringToInteger . takeByteString 31 . blake2b_256 $ transcript0
          transcript1 = transcript0 <> "gamma"
          gamma = Scalar . byteStringToInteger . takeByteString 31 . blake2b_256 $ transcript1
          transcript2 = transcript1 <> "Permutation polynomial" <> bls12_381_G1_compress cz <> "alpha"
          alpha = Scalar . byteStringToInteger . takeByteString 31 . blake2b_256 $ transcript2
          transcript3 = transcript2 <> "Quotient low polynomial" <> bls12_381_G1_compress ctl 
                                    <> "Quotient mid polynomial" <> bls12_381_G1_compress ctm 
                                    <> "Quotient high polynomial" <> bls12_381_G1_compress cth 
                                    <> "zeta"
          zeta = Scalar . byteStringToInteger . takeByteString 31 . blake2b_256 $ transcript3
          transcript4 = transcript3 <> "Append a_eval." <> integerToByteString ea 
                                    <> "Append b_eval." <> integerToByteString eb 
                                    <> "Append c_eval." <> integerToByteString ec 
                                    <> "Append s_sig1." <> integerToByteString es1 
                                    <> "Append s_sig2." <> integerToByteString es2 
                                    <> "Append z_omega." <> integerToByteString ez 
                                    <> "v"
          v = Scalar . byteStringToInteger . takeByteString 31 . blake2b_256 $ transcript4
          transcript5 = transcript4 <> "w_omega comm" <> bls12_381_G1_compress cwo 
                                    <> "w_omega_zeta comm" <> bls12_381_G1_compress cwz 
                                    <> "u"
          u = Scalar . byteStringToInteger . takeByteString 31 . blake2b_256 $ transcript5
          powOfTwoZetaP = powerOfTwoExponentiation zeta p
          powOfTwoZetaPMinOne = powOfTwoZetaP - one
          (lagrangePoly1 : lagrangePolyXs) = zipWith (\x y -> x * powOfTwoZetaPMinOne * y) gens lagsInv 
          piZeta = w1 * lagrangePoly1 + sum (zipWith (*) wxs lagrangePolyXs)
          alphaSquare = alpha * alpha
          alphaEvalZOmega = alpha * evalZOmega
          betaZeta = beta * zeta
          evalAPlusGamma = evalA + gamma
          evalBPlusGamma = evalB + gamma
          evalCPlusGamma = evalC + gamma
          betaEvalS1 = beta * evalS1
          betaEvalS2 = beta * evalS2
          r0 = piZeta - lagrangePoly1*alphaSquare - alphaEvalZOmega*(evalAPlusGamma + betaEvalS1)*(evalBPlusGamma + betaEvalS2)*evalCPlusGamma
          batchPolyCommitG1 = scale (evalA*evalB) qM
                            + scale evalA qL
                            + scale evalB qR
                            + scale evalC qO
                            + qC
                            + scale ((evalAPlusGamma + betaZeta)*(evalBPlusGamma +betaZeta*k1)*(evalCPlusGamma + betaZeta*k2)*alpha + lagrangePoly1*alphaSquare + u) cz
                            - scale ((evalAPlusGamma +betaEvalS1)*(evalBPlusGamma + betaEvalS2)*alphaEvalZOmega*beta) sSig3
                            - scale powOfTwoZetaPMinOne (ctl + scale powOfTwoZetaP ctm + scale (powerOfTwoExponentiation powOfTwoZetaP 1) cth)
          batchPolyCommitFull = batchPolyCommitG1 + scale v (ca + scale v (cb + scale v (cc + scale v (sSig1 + scale v sSig2))))
          groupEncodedBatchEval = scale (negate r0 + v * (evalA + v * (evalB + v * (evalC + v * (evalS1 + v * evalS2)))) + u*evalZOmega ) bls12_381_G1_generator
    in bls12_381_finalVerify 
        (bls12_381_millerLoop (cwo + scale u cwz) x2) 
        (bls12_381_millerLoop (scale zeta cwo + scale (u*zeta*head gens) cwz + batchPolyCommitFull - groupEncodedBatchEval) bls12_381_G2_generator)
       && and (zipWith (\x y -> x * Scalar n * (zeta - y) == one) lagsInv gens)