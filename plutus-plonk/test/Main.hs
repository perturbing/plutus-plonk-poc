{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified PlutusTx.Prelude as P
import qualified Plutus.Crypto.Plonk.Transcript as Plonk
import Plutus.Crypto.BlsField ( Scalar, mkScalar ) 

import Data.Aeson ( FromJSON, ToJSON, decode )
import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy as BL
import Data.ByteString ( pack )
import Data.Word ()
import Plutus.Crypto.Plonk.Transcript (getTranscript)
-- Create a quick type for importing a test vector proof via JSON.
data Proof = Proof 
    { commitment_a     :: [Integer]
    , commitment_b     :: [Integer]
    , commitment_c     :: [Integer]
    , commitment_z     :: [Integer]
    , t_low            :: [Integer]
    , t_mid            :: [Integer]
    , t_high           :: [Integer]
    , w_omega          :: [Integer]
    , w_omega_zeta     :: [Integer]
    , a_eval           :: [Integer]
    , b_eval           :: [Integer]
    , c_eval           :: [Integer]
    , s_sig1           :: [Integer]
    , s_sig2           :: [Integer]
    , z_omega          :: [Integer]
} deriving (Show, Generic)

instance FromJSON Proof 
instance ToJSON Proof 

data PreIn = PreIn 
    { n_public         :: Integer                     
    , power            :: Integer                     
    , k1               :: [Integer]
    , k2               :: [Integer]
    , q_m              :: [Integer]
    , q_l              :: [Integer]
    , q_r              :: [Integer] 
    , q_o              :: [Integer]
    , q_c              :: [Integer]
    , s_sig1_pre_in    :: [Integer]
    , s_sig2_pre_in    :: [Integer]
    , s_sig3_pre_in    :: [Integer]
    , x2               :: [Integer]
    , generator        :: [Integer] 
} deriving (Show, Generic)

instance FromJSON PreIn 
instance ToJSON PreIn

convertIntegerG1Point :: [Integer] -> P.BuiltinBLS12_381_G1_Element
convertIntegerG1Point n = P.bls12_381_G1_uncompress . P.toBuiltin . pack $ Prelude.map fromIntegral n

convertMontgomery :: [Integer] -> Scalar 
convertMontgomery [a, b, c, d] = mkScalar $ a + b * 2^64 + c * 2^128 + d * 2^192

-- a quick test to see if the serialisation between dummy plonk and plutus bls is the same (it is).
main :: IO ()
main = do
    jsonDataProof <- BL.readFile "test-vectors/proof-test-vector.json"
    jsonDataPreIn <- BL.readFile "test-vectors/pre-in-test-vector.json"
    let maybeProof = decode jsonDataProof :: Maybe Proof 
    let maybePreIn = decode jsonDataPreIn :: Maybe PreIn
    case maybeProof of
        Just proof -> do let a = convertIntegerG1Point $ commitment_a proof
                         let b = convertIntegerG1Point $ commitment_b proof
                         let c = convertIntegerG1Point $ commitment_c proof
                         let z = convertIntegerG1Point $ commitment_z proof
                         let tLow = convertIntegerG1Point $ t_low proof
                         let tMid = convertIntegerG1Point $ t_mid proof
                         let tHigh = convertIntegerG1Point $ t_high proof
                         let aEval = convertMontgomery $ a_eval proof
                         let bEval = convertMontgomery $ b_eval proof
                         let cEval = convertMontgomery $ c_eval proof
                         let sSig1 = convertMontgomery $ s_sig1 proof
                         let sSig2 = convertMontgomery $ s_sig2 proof
                         let zOmega = convertMontgomery $ z_omega proof
                         let wOmega = convertIntegerG1Point $ w_omega proof
                         let wOmegaZeta = convertIntegerG1Point $ w_omega_zeta proof
                         let (beta, gamma, alpha, zeta, v, u) = getTranscript a b c z tLow tMid tHigh aEval bEval cEval sSig1 sSig2 zOmega wOmega wOmegaZeta
                         print u
                         case maybePreIn of
                            -- test is the generator is indeed the correct generator as in the dummy plonk implementation
                            Just preIn -> print $ convertMontgomery (generator preIn)
                            Nothing -> putStrLn "Failed to parse preIn JSON"
        Nothing -> putStrLn "Failed to parse proof JSON."
