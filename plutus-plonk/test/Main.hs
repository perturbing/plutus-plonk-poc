{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified PlutusTx.Prelude as P
import qualified Plutus.Crypto.Plonk.Transcript as Plonk

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
                         let beta = Plonk.challengeScalar transcript3 "beta"
                         print beta
        Nothing -> putStrLn "Failed to parse JSON."