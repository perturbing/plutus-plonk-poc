{-# LANGUAGE DeriveGeneric      #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module PlutusBenchmark.Verifier.RunVerifier
( runVerifier
) where

import PlutusBenchmark.Verifier.Scripts (verifyPlonkFastScript, verifyPlonkScript)
import Plutus.Crypto.Plonk (PreInputsFast, ProofFast, Proof (..), PreInputs (..)
                           , convertToFastProof, convertToFastPreInputs)
import Plutus.Crypto.BlsField (mkScalar)

import PlutusBenchmark.Common ( printHeader, printSizeStatistics, TestSize(NoSize) )

import qualified PlutusTx.Prelude as P

import System.IO (Handle)
import Text.Printf (hPrintf)

import Data.Aeson ( FromJSON, ToJSON, decode )
import GHC.Generics ( Generic )

import qualified Data.ByteString.Lazy as BL
import Data.ByteString ( pack )
import Data.Word ()

printCostsVerifierPlonkFast :: Handle -> PreInputsFast -> [Integer] -> ProofFast -> IO ()
printCostsVerifierPlonkFast h preIn pub proof =
    let script = verifyPlonkFastScript preIn pub proof
    in printSizeStatistics h NoSize script

printCostsVerifierPlonk :: Handle -> PreInputs -> [Integer] -> Proof -> IO ()
printCostsVerifierPlonk h preIn pub proof =
    let script = verifyPlonkScript preIn pub proof
    in printSizeStatistics h NoSize script

runVerifier :: Handle -> IO ()
runVerifier h = do
    jsonDataProof <- BL.readFile "test-vectors/proof-test-vector.json"
    jsonDataPreIn <- BL.readFile "test-vectors/pre-in-test-vector.json"
    let maybeProof = decode jsonDataProof :: Maybe ProofJSON
    let maybePreIn = decode jsonDataPreIn :: Maybe PreInputsJSON
    case maybeProof of
        Just proof  -> case maybePreIn of
            Just preIn -> do let p = convertProof proof
                             let i = convertPreInputs preIn
                             hPrintf h "\n\n"
                             hPrintf h "Run slow vanilla plonk verifier\n\n"
                             printHeader h
                             printCostsVerifierPlonk h i [9] p
                             hPrintf h "\n\n"
                             let iFast = convertToFastPreInputs i
                             let pFast = convertToFastProof iFast p
                             hPrintf h "\n\n"
                             hPrintf h "Run fast vanilla plonk verifier\n\n"
                             printHeader h
                             printCostsVerifierPlonkFast h iFast [9] pFast
                             hPrintf h "\n\n"
            Nothing -> print "Could not deserialize PreInputs test vector"
        Nothing -> print "Could not deserialize Proof test vector"

-- Create a quick type for importing a test vector Proof via JSON.
data ProofJSON = ProofJSON 
    { commitment_a :: [Integer]
    , commitment_b :: [Integer]
    , commitment_c :: [Integer]
    , commitment_z :: [Integer]
    , t_low        :: [Integer]
    , t_mid        :: [Integer]
    , t_high       :: [Integer]
    , w_omega      :: [Integer]
    , w_omega_zeta :: [Integer]
    , a_eval       :: [Integer]
    , b_eval       :: [Integer]
    , c_eval       :: [Integer]
    , s_sig1       :: [Integer]
    , s_sig2       :: [Integer]
    , z_omega      :: [Integer]
} deriving (Show, Generic)

instance FromJSON ProofJSON
instance ToJSON ProofJSON

-- Create a quick type for importing a test vector PreInputs via JSON.
data PreInputsJSON = PreInputsJSON 
    { n_public      :: Integer                     
    , pow           :: Integer                     
    , k_1           :: [Integer]
    , k_2           :: [Integer]
    , q_m           :: [Integer]
    , q_l           :: [Integer]
    , q_r           :: [Integer] 
    , q_o           :: [Integer]
    , q_c           :: [Integer]
    , s_sig1_pre_in :: [Integer]
    , s_sig2_pre_in :: [Integer]
    , s_sig3_pre_in :: [Integer]
    , x_2           :: [Integer]
    , gen           :: [Integer] 
} deriving (Show, Generic)

instance FromJSON PreInputsJSON 
instance ToJSON PreInputsJSON

convertIntegersByteString :: [Integer] -> P.BuiltinBLS12_381_G1_Element
convertIntegersByteString n = P.bls12_381_G1_uncompress .  P.toBuiltin . pack $ Prelude.map fromIntegral n

convertIntegersByteStringG2 :: [Integer] -> P.BuiltinBLS12_381_G2_Element
convertIntegersByteStringG2 n = P.bls12_381_G2_uncompress .  P.toBuiltin . pack $ Prelude.map fromIntegral n

convertMontgomery :: [Integer] -> Integer
convertMontgomery [a, b, c, d] = a + b * 2^64 + c * 2^128 + d * 2^192
convertMontgomery _ = 0

convertProof :: ProofJSON -> Proof
convertProof proof = Proof
    { commitmentA = convertIntegersByteString $ commitment_a proof
    , commitmentB = convertIntegersByteString $ commitment_b proof
    , commitmentC = convertIntegersByteString $ commitment_c proof
    , commitmentZ = convertIntegersByteString $ commitment_z proof
    , tLow        = convertIntegersByteString $ t_low proof
    , tMid        = convertIntegersByteString $ t_mid proof
    , tHigh       = convertIntegersByteString $ t_high proof
    , wOmega      = convertIntegersByteString $ w_omega proof
    , wOmegaZeta  = convertIntegersByteString $ w_omega_zeta proof
    , aEval       = convertMontgomery $ a_eval proof
    , bEval       = convertMontgomery $ b_eval proof
    , cEval       = convertMontgomery $ c_eval proof
    , sSig1P      = convertMontgomery $ s_sig1 proof
    , sSig2P      = convertMontgomery $ s_sig2 proof
    , zOmega      = convertMontgomery $ z_omega proof
}

convertPreInputs :: PreInputsJSON -> PreInputs
convertPreInputs preIn = PreInputs
    { nPublic   = n_public preIn
    , power     = pow preIn
    , k1        = mkScalar . convertMontgomery $ k_1 preIn
    , k2        = mkScalar . convertMontgomery $ k_2 preIn
    , qM        = convertIntegersByteString $ q_m preIn
    , qL        = convertIntegersByteString $ q_l preIn 
    , qR        = convertIntegersByteString $ q_r preIn
    , qO        = convertIntegersByteString $ q_o preIn
    , qC        = convertIntegersByteString $ q_c preIn
    , sSig1     = convertIntegersByteString $ s_sig1_pre_in preIn
    , sSig2     = convertIntegersByteString $ s_sig2_pre_in preIn
    , sSig3     = convertIntegersByteString $ s_sig3_pre_in preIn
    , x2        = convertIntegersByteStringG2 $ x_2 preIn 
    , generator = mkScalar . convertMontgomery $ gen preIn
    }