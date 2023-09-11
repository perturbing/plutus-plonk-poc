{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.Crypto.Plonk.Transcript 
( Transcript
, Label
, transcriptNew
, transcriptAppendMsg
, transcriptPoint
, challengeScalar
) where

import PlutusTx.Prelude ( BuiltinByteString, id, (<>), lengthOfByteString, dropByteString, Integer, ($), (.) )
                          
import PlutusTx.Builtins (BuiltinBLS12_381_G1_Element (..), bls12_381_G1_compress, blake2b_256)
import Plutus.Crypto.Number.Serialize ( i2osp, os2ip )
import Plutus.Crypto.BlsField
import PlutusTx.Numeric

-- create type synonym for type safety
type Transcript = BuiltinByteString
type Label = BuiltinByteString

-- These salted transcripts assume that the dummy-plonk implementation appends and not prepends.
-- see https://github.com/iquerejeta/dummy_plonk/blob/main/src/transcript.rs

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

-- Note that the digest lays in the full 256 bit domain, while a scalar
-- is bound by the field prime. That is why we cut of the most significant byte
-- to make this function well-defined.
{-# INLINEABLE challengeScalar #-}
challengeScalar :: Transcript -> Label -> Scalar
challengeScalar ts lbl = mkScalar . os2ip . dropByteString 1 . blake2b_256 $ ts <> lbl