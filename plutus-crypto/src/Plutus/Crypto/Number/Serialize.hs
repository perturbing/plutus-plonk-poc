{-# LANGUAGE NoImplicitPrelude #-}

module Plutus.Crypto.Number.Serialize 
( i2osp
, os2ip
, i2ospOf
, i2ospOf_
, lengthBytes
) where

import PlutusTx.Prelude
    ( otherwise,
      Integer,
      Maybe(..),
      consByteString,
      emptyByteString,
      error,
      indexByteString,
      lengthOfByteString,
      divide,
      even,
      modulo,
      quotient,
      remainder,
      BuiltinByteString,
      Eq((==)),
      AdditiveGroup((-)),
      AdditiveSemigroup((+)),
      MultiplicativeSemigroup((*)),
      Ord((<), (>), (>=)),
      Semigroup((<>)) )

import Plutus.Crypto.Number.ModArithmetic ( exponentiate )

-- | i2osp converts a positive integer into a builtin byte string
--   Plutus version of `(Crypto.Number.Serialize.i2osp)`
--   As per rfc3447, the first byte is the most significant byte.
--   This function will give an error for a negative integer.
{-# INLINABLE i2osp #-}
i2osp :: Integer -> BuiltinByteString
i2osp n
    | n < 0     = error ()
    | n == 0    = consByteString 0 emptyByteString
    | otherwise = go n
    where go m 
            | m == 0    = emptyByteString
            | otherwise = go (m `quotient` 256) <> consByteString (m `remainder` 256) emptyByteString

-- | os2ip converts a builtin byte string into a positive integer
--   Plutus version of `(Crypto.Number.Serialize.os2ip)`
--   As per rfc3447, the first byte is the most significant byte
--   This function will give an error for an empty builtin byte string
{-# INLINABLE os2ip #-}
os2ip :: BuiltinByteString -> Integer
os2ip b = go 1
    where len = lengthOfByteString b
          go i =
              if i > len
              then 0
              else indexByteString b (len-i) + 256 * go (i + 1)

-- | Just like i2osp, but take an extra parameter for size. 
--   if the number is too big to fit in len bytes, nothing is returned 
--   otherwise the number is padded with 0 to fit the len required. 
--   Plutus version of `(Crypto.Number.Serialize.i2ospOf)`
{-# INLINABLE i2ospOf #-}
i2ospOf :: Integer -> Integer -> Maybe BuiltinByteString
i2ospOf len n 
    | n >= 256 `exponentiate` len   = Nothing
    | otherwise                     = Just (nullPadding (len - lengthOfByteString bs) <> bs)
    where bs = i2osp n

-- | Just like i2ospOf except that it doesn't expect a failure:
--   i.e. an integer larger than the number of output bytes requested
--   for example if you just took a modulo of the number that represent the size 
--   Plutus version of `(Crypto.Number.Serialize.i2ospOf_)`
{-# INLINEABLE i2ospOf_ #-}
i2ospOf_ :: Integer -> Integer -> BuiltinByteString
i2ospOf_ len n = nullPadding (len - lengthOfByteString bs) <> bs
    where bs = i2osp (n `modulo` (256 `exponentiate` len))

-- | Returns the number of bytes that are needed store an integer with i2osp
--   Plutus version of `(Crypto.Number.Serialize.lengthBytes)`
{-# INLINEABLE lengthBytes #-}
lengthBytes :: Integer -> Integer
lengthBytes n = go 1 n 
    where go acc n
            | n < 256 `exponentiate` acc    = acc
            | otherwise                     = go (acc+1) n

-- | generate a builtin byte string of "\null" bytes of length n
{-# INLINEABLE nullPadding #-}
nullPadding :: Integer -> BuiltinByteString
nullPadding n = go n (consByteString 0 emptyByteString)
    where go n bs
            | n == 0      = emptyByteString
            | even n      = go (n `divide` 2) bs <> go (n `divide` 2) bs
            | otherwise   = go ((n-1) `divide` 2) bs <> go ((n-1) `divide` 2) bs <> bs