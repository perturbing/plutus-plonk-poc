{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main (main) where

import qualified PlutusTx as P
import qualified PlutusTx.Prelude as P
import qualified PlutusLedgerApi.V3 as PlutusV3
import UntypedPlutusCore (UnrestrictedProgram (..))

import Script (verifyPlonkCode)
import Plutus.Crypto.BlsField ( mkScalar ) 
import Plutus.Crypto.Plonk.Inputs (Proof (..), PreInputs (..))

import Data.Aeson ( FromJSON, ToJSON, decode )
import Flat (flat)
import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy as BL
import Data.ByteString as BS
import Data.Word ()
import qualified PlutusTx as Tx

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

convertIntegersByteString :: [Integer] -> P.BuiltinByteString
convertIntegersByteString n =  P.toBuiltin . BS.pack $ Prelude.map fromIntegral n

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
    , qM        = P.bls12_381_G1_uncompress . convertIntegersByteString $ q_m preIn
    , qL        = P.bls12_381_G1_uncompress . convertIntegersByteString $ q_l preIn 
    , qR        = P.bls12_381_G1_uncompress . convertIntegersByteString $ q_r preIn
    , qO        = P.bls12_381_G1_uncompress . convertIntegersByteString $ q_o preIn
    , qC        = P.bls12_381_G1_uncompress . convertIntegersByteString $ q_c preIn
    , sSig1     = P.bls12_381_G1_uncompress . convertIntegersByteString $ s_sig1_pre_in preIn
    , sSig2     = P.bls12_381_G1_uncompress . convertIntegersByteString $ s_sig2_pre_in preIn
    , sSig3     = P.bls12_381_G1_uncompress . convertIntegersByteString $ s_sig3_pre_in preIn
    , x2        = P.bls12_381_G2_uncompress . convertIntegersByteString $ x_2 preIn 
    , generator = mkScalar . convertMontgomery $ gen preIn
    }

main :: IO ()
main = do
    jsonDataProof <- BL.readFile "test-vectors/proof-test-vector.json"
    jsonDataPreIn <- BL.readFile "test-vectors/pre-in-test-vector.json"
    let maybeProof = decode jsonDataProof :: Maybe ProofJSON
    let maybePreIn = decode jsonDataPreIn :: Maybe PreInputsJSON
    case maybeProof of
        Just proof  -> case maybePreIn of
            Just preIn -> do let p = convertProof proof
                             let i = convertPreInputs preIn
                             BS.writeFile "appliedPlonkScript.flat" . flat . UnrestrictedProgram <$> P.getPlcNoAnn $ verifyPlonkCode
                                `Tx.unsafeApplyCode` Tx.liftCodeDef i
                                `Tx.unsafeApplyCode` Tx.liftCodeDef [9]
                                `Tx.unsafeApplyCode` Tx.liftCodeDef p
            Nothing -> print "Could not deserialize PreInputs test vector"
        Nothing -> print "Could not deserialize Proof test vector"