{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.Crypto.Plonk.Transcript 
( Transcript
, Label
, transcriptNew
, transcriptAppendMsg
, transcriptScalar
, transcriptPoint
, challengeScalar
, getTranscript
) where

import PlutusTx.Prelude ( BuiltinByteString, id, (<>), lengthOfByteString, dropByteString, Integer, ($), (.) )
import PlutusTx.Builtins (BuiltinBLS12_381_G1_Element (..), bls12_381_G1_compress, blake2b_256)
import Plutus.Crypto.Number.Serialize ( i2osp, os2ip )
import Plutus.Crypto.BlsField ( mkScalar, Scalar(..) )

-- For information on the particular chosen salts below,
-- see https://github.com/iquerejeta/dummy_plonk/blob/main/src/transcript.rs

-- create type synonym for type safety
type Transcript = BuiltinByteString
type Label = BuiltinByteString

{-# INLINEABLE transcriptNew #-}
transcriptNew :: Label -> Transcript
transcriptNew lbl = "FS transcript" <> "dom-sep" <> lbl

{-# INLINEABLE transcriptAppendMsg #-}
transcriptAppendMsg :: Transcript -> Label -> BuiltinByteString -> Transcript
transcriptAppendMsg ts lbl msg = ts <> lbl <> i2osp (lengthOfByteString msg) <> msg

{-# INLINEABLE transcriptPoint #-}
transcriptPoint :: Transcript -> Label -> BuiltinBLS12_381_G1_Element -> Transcript
transcriptPoint ts lbl pnt = ts <> lbl <> bls12_381_G1_compress pnt

{-# INLINEABLE transcriptScalar #-}
transcriptScalar :: Transcript -> Label -> Scalar -> Transcript
transcriptScalar ts lbl scl = ts <> lbl <> i2osp (unScalar scl)

-- Note that the hash digest maps into the 256 bit domain, while a scalar
-- is bound by the 255 bit field prime. 
-- That is why we cut of the most significant byte
-- to make this function well-defined.
{-# INLINEABLE challengeScalar #-}
challengeScalar :: Transcript -> Label -> (Scalar,Transcript)
challengeScalar ts lbl = (mkScalar . os2ip . dropByteString 1 . blake2b_256 $ newTs, newTs)
    where newTs = ts <> lbl

-- Given the necesary values of a proof,
-- calcualate the transript values. These values 
-- make the proof non interactive.
-- 
-- TODO: Disuss with team how we can chose
--       these values to be more consice.
--
{-# INLINABLE getTranscript #-}
getTranscript
    :: BuiltinBLS12_381_G1_Element -- commitment_a
    -> BuiltinBLS12_381_G1_Element -- commitment_b
    -> BuiltinBLS12_381_G1_Element -- commitment_c
    -> BuiltinBLS12_381_G1_Element -- commitment_z
    -> BuiltinBLS12_381_G1_Element -- t_low
    -> BuiltinBLS12_381_G1_Element -- t_mid
    -> BuiltinBLS12_381_G1_Element -- t_high
    -> Scalar                      -- a_eval
    -> Scalar                      -- b_eval
    -> Scalar                      -- c_eval
    -> Scalar                      -- s_sig1
    -> Scalar                      -- s_sig2
    -> Scalar                      -- z_omega
    -> BuiltinBLS12_381_G1_Element -- w_omega
    -> BuiltinBLS12_381_G1_Element -- w_omega_zeta
    -> (Scalar,Scalar,Scalar,Scalar,Scalar,Scalar) -- (beta, gamma, alpha, zeta, v, u
getTranscript commA commB commC commZ commTLow commTMid commTHigh evalA evalB evalC evalS1 evalS2 evalZOmega commWOmega commWOmegaZeta =
    let
        (beta, transcript1) = challengeScalar (transcriptPoint (transcriptPoint (transcriptPoint (transcriptNew "testing the prover") "commitment a" commA) "commitment b" commB) "commitment c" commC) "beta"
        (gamma, transcript2) = challengeScalar transcript1 "gamma"
        (alpha,transcript3) = challengeScalar (transcriptPoint transcript2 "Permutation polynomial" commZ) "alpha"
        (zeta, transcript4) = challengeScalar (transcriptPoint (transcriptPoint (transcriptPoint transcript3 "Quotient low polynomial" commTLow) "Quotient mid polynomial" commTMid) "Quotient high polynomial" commTHigh) "zeta"
        (v, transcript5) = challengeScalar (transcriptScalar (transcriptScalar (transcriptScalar (transcriptScalar (transcriptScalar (transcriptScalar transcript4 "Append a_eval." evalA) "Append b_eval." evalB) "Append c_eval." evalC) "Append s_sig1." evalS1) "Append s_sig2." evalS2) "Append z_omega." evalZOmega) "v"
        (u, _) = challengeScalar (transcriptPoint (transcriptPoint transcript5 "w_omega comm" commWOmega) "w_omega_zeta comm" commWOmegaZeta) "u"
    in
        (beta, gamma, alpha, zeta, v, u)