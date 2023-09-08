{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import qualified PlutusTx.Prelude as P
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

-- a quick test to see if the serialisation between dummy plonk and plutus bls is the same (it is).
main :: IO ()
main = do
    jsonData <- BL.readFile "test-vectors/proof-test-vector.json"
    let maybeProof = decode jsonData :: Maybe Proof 
    case maybeProof of
        Just proof -> do let a = commitment_a proof 
                         let aWord8 = Prelude.map fromIntegral a :: [Word8]
                         -- print the first committed point a as a plutus builtin g1 point.
                         print $ P.bls12_381_G1_uncompress $ P.toBuiltin $ pack aWord8
        Nothing -> putStrLn "Failed to parse JSON."