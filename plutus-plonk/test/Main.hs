{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified PlutusTx.Prelude as P
import qualified Plutus.Crypto.Plonk.Transcript as Plonk
import Plutus.Crypto.BlsField 

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as BL
import Data.ByteString
import Data.Word

-- Create a quick type for importing a test vector proof via JSON.
data Proof = Proof {
    commitment_a     :: [Integer],
    commitment_b     :: [Integer],
    commitment_c     :: [Integer],
    commitment_z     :: [Integer],
    t_low            :: [Integer],
    t_mid            :: [Integer],
    t_high           :: [Integer],
    w_omega          :: [Integer],
    w_omega_zeta     :: [Integer],
    a_eval           :: [Integer],
    b_eval           :: [Integer],
    c_eval           :: [Integer],
    s_sig1           :: [Integer],
    s_sig2           :: [Integer],
    z_omega          :: [Integer]
} deriving (Show, Generic)

instance FromJSON Proof 
instance ToJSON Proof 

convertIntegerG1Point :: [Integer] -> P.BuiltinBLS12_381_G1_Element
convertIntegerG1Point n = P.bls12_381_G1_uncompress . P.toBuiltin . pack $ Prelude.map fromIntegral n

-- a quick test to see if the serialisation between dummy plonk and plutus bls is the same (it is).
main :: IO ()
main = do
    jsonData <- BL.readFile "test-vectors/proof-test-vector.json"
    let maybeProof = decode jsonData :: Maybe Proof 
    case maybeProof of
        Just proof -> do let a = convertIntegerG1Point $ commitment_a proof
                         let b = convertIntegerG1Point $ commitment_b proof
                         let c = convertIntegerG1Point $ commitment_c proof
                         let transcript0 = Plonk.transcriptNew "testing the prover"
                         let transcript1 = Plonk.transcriptPoint transcript0 "commitment a" a
                         let transcript2 = Plonk.transcriptPoint transcript1 "commitment b" b
                         let transcript3 = Plonk.transcriptPoint transcript2 "commitment c" c
                         let (beta,transcript4) = Plonk.challengeScalar transcript3 "beta"
                         let (gamma,transcript6) = Plonk.challengeScalar transcript4 "gamma"
                         let z = convertIntegerG1Point $ commitment_z proof
                         let transcript7 = Plonk.transcriptPoint transcript6 "Permutation polynomial" z
                         let (alpha,transcript8) = Plonk.challengeScalar transcript7 "alpha"
                         let tLow = convertIntegerG1Point $ t_low proof
                         let tMid = convertIntegerG1Point $ t_mid proof
                         let tHigh = convertIntegerG1Point $ t_high proof
                         let transcript9 = Plonk.transcriptPoint transcript8 "Quotient low polynomial" tLow
                         let transcript10 = Plonk.transcriptPoint transcript9 "Quotient mid polynomial" tMid
                         let transcript11 = Plonk.transcriptPoint transcript10 "Quotient high polynomial" tHigh
                         let (zeta,transcript12) = Plonk.challengeScalar transcript11 "zeta"
                         let aEval = convertMontgomery $ a_eval proof
                         let bEval = convertMontgomery $ b_eval proof
                         let cEval = convertMontgomery $ c_eval proof
                         let sSig1 = convertMontgomery $ s_sig1 proof
                         let sSig2 = convertMontgomery $ s_sig2 proof
                         let zOmega = convertMontgomery $ z_omega proof
                         let transcript13 = Plonk.transcriptScalar transcript12 "Append a_eval." aEval
                         let transcript14 = Plonk.transcriptScalar transcript13 "Append b_eval." bEval
                         let transcript15 = Plonk.transcriptScalar transcript14 "Append c_eval." cEval
                         let transcript16 = Plonk.transcriptScalar transcript15 "Append s_sig1." sSig1
                         let transcript17 = Plonk.transcriptScalar transcript16 "Append s_sig2." sSig2
                         let transcript18 = Plonk.transcriptScalar transcript17 "Append z_omega." zOmega
                         let (v,transcript19) = Plonk.challengeScalar transcript18 "v"
                         print v
                         let wOmega = convertIntegerG1Point $ w_omega proof
                         let wOmegaZeta = convertIntegerG1Point $ w_omega_zeta proof
                         let transcript20 = Plonk.transcriptPoint transcript19 "w_omega comm" wOmega
                         let transcript21 = Plonk.transcriptPoint transcript20 "w_omega_zeta comm" wOmegaZeta
                         let (u,_) = Plonk.challengeScalar transcript21 "u"
                         print u
        Nothing -> putStrLn "Failed to parse JSON."

convertMontgomery :: [Integer] -> Scalar 
convertMontgomery [a, b, c, d] = mkScalar $ a + b * 2^64 + c * 2^128 + d * 2^192
