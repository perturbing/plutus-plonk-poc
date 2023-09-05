{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.Crypto.Plonk.Transcript (
    Transcript
  , Label
  , transcriptNew
) where

import PlutusTx.Prelude ( BuiltinByteString, id, (<>), lengthOfByteString)

-- TODO: add interface that, given a proof, gives the transcript to make the proof non interactive.

-- General question,
-- How much is needed of the appending of all these salts?

type Transcript = BuiltinByteString
type Label = BuiltinByteString

-- This assumes that the dummy-plonk implementation appends.
{-# INLINEABLE transcriptNew #-}
transcriptNew :: Label -> Transcript
transcriptNew lbl = "FS transcript" <> "dom-sep" <> lbl

-- {-# INLINEABLE transcriptAppendMsg #-}
-- transcriptAppendMsg:: Transcript -> Label -> BuiltinByteString -> Transcript
-- transcriptAppendMsg ts lbl msg = ts <> lbl <> lengthOfByteString msg <> msg
