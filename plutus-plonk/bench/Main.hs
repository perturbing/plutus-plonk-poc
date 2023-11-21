{-# LANGUAGE DeriveGeneric          #-}
{-# OPTIONS_GHC -Wno-type-defaults  #-}

module Main (main) where

import qualified PlutusTx as P
import qualified PlutusTx.Prelude as P
import UntypedPlutusCore (UnrestrictedProgram (..))

import Script (verifyPlonkCode)
import Plutus.Crypto.BlsField ( mkScalar ) 
import Plutus.Crypto.Plonk (Proof (..), PreInputs (..), convertToFastProof, convertToFastPreInputs)

import Data.Aeson ( FromJSON, ToJSON, decode )
import Flat (flat)
import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy as BL
import Data.ByteString as BS ( pack, writeFile )
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
convertIntegersByteString n = P.toBuiltin . BS.pack $ Prelude.map fromIntegral n

convertIntegersByteStringG2 :: [Integer] -> P.BuiltinByteString
convertIntegersByteStringG2 n = P.toBuiltin . BS.pack $ Prelude.map fromIntegral n

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

-- This reads the test vectors and applies them to the compiled plonk verifier script.
-- This applied verifier is written to disk in the flat format.
-- use: nix shell nixpkgs#flamegraph github:input-output-hk/plutus#x86_64-linux.plutus.library.plutus-project-92.hsPkgs.plutus-core.components.exes.traceToStacks github:input-output-hk/plutus#x86_64-linux.plutus.library.plutus-project-92.hsPkgs.plutus-core.components.exes.uplc
-- use: nix shell nixpkgs#flamegraph github:perturbing/plutus/8152f746f07c2097ef18bd649e55a6269d6cb47b#x86_64-linux.plutus.library.plutus-project-92.hsPkgs.plutus-core.components.exes.traceToStacks github:perturbing/plutus/8152f746f07c2097ef18bd649e55a6269d6cb47b#x86_64-linux.plutus.library.plutus-project-92.hsPkgs.plutus-core.components.exes.uplc
-- and: uplc evaluate -t -i appliedPlonkScript.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs
-- to log the CPU/MEM consumption
-- for more info see https://hydra.family/head-protocol/benchmarks/profiling/
-- and https://plutus.readthedocs.io/en/latest/howtos/profiling-scripts.html
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
                             let iFast = convertToFastPreInputs i
                             let pFast = convertToFastProof iFast p
                             BS.writeFile "appliedPlonkScript.flat" . flat . UnrestrictedProgram <$> P.getPlcNoAnn $ verifyPlonkCode
                                `Tx.unsafeApplyCode` Tx.liftCodeDef iFast
                                `Tx.unsafeApplyCode` Tx.liftCodeDef [9]
                                `Tx.unsafeApplyCode` Tx.liftCodeDef pFast
            Nothing -> print "Could not deserialize PreInputs test vector"
        Nothing -> print "Could not deserialize Proof test vector"