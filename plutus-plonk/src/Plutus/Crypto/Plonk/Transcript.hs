{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.Crypto.Plonk.Transcript (
    Transcript
  , Label
  , transcriptNew
  , transcriptAppendMsg
  , transcriptPoint
) where

import PlutusTx.Prelude ( BuiltinByteString, id, (<>), lengthOfByteString, Integer, ($), (.) )
                          
import PlutusTx.Builtins (BuiltinBLS12_381_G1_Element (..), bls12_381_G1_compress, blake2b_256)
import Plutus.Crypto.Number.Serialize ( i2osp, os2ip )

-- TODO: add interface that, given a proof, gives the transcript to make the proof non interactive.

-- General question,
-- How much is needed of the appending of all these salts?

type Transcript = BuiltinByteString
type Label = BuiltinByteString

-- This assumes that the dummy-plonk implementation appends.
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

{-# INLINEABLE challangeScalar #-}
challangeScalar :: Transcript -> Label -> Integer
challangeScalar ts lbl = os2ip . blake2b_256 $ ts <> lbl