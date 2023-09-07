{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.Crypto.Plonk.Transcript (
    Transcript
  , Label
  , transcriptNew
  , transcriptAppendMsg
  , transcriptPoint
  , challengeScalar
) where

import PlutusTx.Prelude ( BuiltinByteString, id, (<>), lengthOfByteString, Integer, ($), (.) )
                          
import PlutusTx.Builtins (BuiltinBLS12_381_G1_Element (..), bls12_381_G1_compress, blake2b_256)
import Plutus.Crypto.Number.Serialize ( i2osp, os2ip )

-- TODO: add interface that, given a proof, gives the transcript to make the proof non interactive.
-- TODO: Current implementation below uses Integers, change this to Scalars (field elements, see bls-utils)

-- General question/notes to self
-- How much is needed of the appending of all these (large) salts? Will letters suffice?
-- The field module implementatation (a ring plus the scalar additive group)

-- create type synonym for type safty
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
transcriptScalar :: Transcript -> Label -> Integer -> Transcript
transcriptScalar ts lbl scl = ts <> lbl <> i2osp scl

{-# INLINEABLE challengeScalar #-}
challengeScalar :: Transcript -> Label -> Integer
challengeScalar ts lbl = os2ip . blake2b_256 $ ts <> lbl