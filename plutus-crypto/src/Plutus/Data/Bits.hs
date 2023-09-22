{-# LANGUAGE NoImplicitPrelude #-}
-- {-# OPTIONS_GHC -fno-specialise #-}

module Plutus.Data.Bits (
-- * conversion functions
  intToByteBE
, byteBEToInt
, intToByteLE
, byteLEToInt
-- * inversion functions
, reverseBS
-- * bit operation on builtin byte strings
, setBit
, clearBit
, testBit
--,shiftBits -- TODO: make this function for fun (negative and positive shift)
) where

import PlutusTx.Prelude
import Plutus.Crypto.Number.ModArithmetic

-- | add the element False n times to an empty list
--   
--   Note that this function is unsafe for negative integer.
--   Since it is used internally in this module no error is thrown
boolPadding :: Integer -> [Bool]
boolPadding n
    | n==0      = []
    | otherwise = False : boolPadding (n-1)
{-# INLINABLE boolPadding #-}

-- | A visual representation of a bytestring for a low level view (little endian)
--   Note that for a negative integer this does not terminate.
--   Since it is used internally in this module no error is thrown.
intToBitsLE :: Integer -> [Bool]
intToBitsLE n
    | n == 0 = []
    | otherwise = (n `remainder` 2 == 1) : intToBitsLE (n `quotient` 2)
{-# INLINABLE intToBitsLE #-}

-- | Convert an positive integer smaller than 256 into its little endian
--   representation viewed as a list of bools.
--   This function will give an error if the integer is negative or bigger.
intToByteLE :: Integer -> [Bool]
intToByteLE n
    | n > 255 || n < 0  = error ()
    | otherwise         = xs ++ boolPadding (8 - length xs)
    where xs = intToBitsLE n
{-# INLINABLE intToByteLE #-}

-- | Convert bits represented as a list of bools into an integer (little endian)
bitsLEToInt :: [Bool] -> Integer
bitsLEToInt = go 0
    where
        go _ [] = error ()
        go n (x:xs)
            | null xs  = if x then 2 `exponentiate` n else 0
            | x         = 2 `exponentiate` n + go (n+1) xs
            | otherwise = go (n+1) xs
{-# INLINABLE bitsLEToInt #-}

-- | Convert a byte represented as a list of 8 bools into an integer (little endian)
--   This function will fail if the length of the list is not 8.
byteLEToInt :: [Bool] -> Integer
byteLEToInt xs
    | length xs /= 8    = error ()
    | otherwise         = bitsLEToInt xs
{-# INLINABLE byteLEToInt #-}

-- | A visual representation of a bytestring for a low level view (big endian)
--   Note that for a negative integer this does not terminate.
--   Since it is used internally in this module no error is thrown.
intToBitsBE :: Integer -> [Bool]
intToBitsBE n
    | n == 0 = []
    | otherwise = intToBitsBE (n `quotient` 2) ++ [n `remainder` 2 == 1 ]
{-# INLINABLE intToBitsBE #-}

-- | Convert an positive integer smaller than 256 into its big endian
--   representation viewed as a list of bools.
--   This function will give an error if the integer is negative or bigger.
intToByteBE :: Integer -> [Bool]
intToByteBE n
    | n > 255 || n < 0  = error ()
    | otherwise         = boolPadding (8 - length xs) ++ xs
    where xs = intToBitsBE n
{-# INLINABLE intToByteBE #-}

-- | Convert bits represented as a list of bools into an integer (big endian)
bitsBEToInt :: [Bool] -> Integer
bitsBEToInt = bitsLEToInt . reverse
{-# INLINABLE bitsBEToInt #-}

-- | Convert a byte represented as a list of 8 bools into an integer (big endian)
--   This function will give an error if the length of the list is not 8.
byteBEToInt :: [Bool] -> Integer
byteBEToInt xs
    | length xs /= 8    = error ()
    | otherwise         = bitsBEToInt xs
{-# INLINABLE byteBEToInt #-}

-- | Reverse a byte in its integer representation.
--   So 1 -> 128, 2->64, 4->32, 8->16, 16->8 ...
--
--   This function will throw an error for negative and 
--   intigers above 255.
--   Since it is used internally in this module no error is thrown.
reverseByte :: Integer -> Integer
reverseByte = byteLEToInt . intToByteBE
{-# INLINABLE reverseByte #-}

-- | Reverse a builtin byte string of arbitrary length
reverseBS :: BuiltinByteString -> BuiltinByteString
reverseBS bs
    | bs == emptyByteString = bs
    | otherwise             = reverseBS (dropByteString 1 bs) <> consByteString (reverseByte (indexByteString bs 0)) emptyByteString
{-# INLINABLE reverseBS #-}

-- -- | Plutus Tx version of 'Data.List.splitAt'.
-- splitAt :: Integer -> [a] -> ([a], [a])
-- splitAt n xs
--   | n <= 0    = ([], xs)
--   | otherwise = go n xs
--   where
--     go :: Integer -> [a] -> ([a], [a])
--     go _ []     = ([], [])
--     go m (y:ys)
--       | m == 1 = ([y], ys)
--       | otherwise = case go (m-1) ys of
--           (zs, ws) -> (y:zs, ws)
-- {-# INLINABLE splitAt #-}

-- | set bit n in a byte represented as a list of bools.
--   This function acts from the left, so zero correponds
--   to the left most bool in the list.
--
--   This function is unsafe for negative integers and
--   integers bigger than 7.
--   Since it is used internally in this module no error is thrown.
setBit' :: [Bool] -> Integer -> [Bool]
setBit' xs n = prefix ++ [True] ++ tail suffix
    where (prefix, suffix) = splitAt n xs
{-# INLINABLE setBit' #-}

-- | clear bit n in a byte represented as a list of bools.
--   This function acts from the left, so zero correponds
--   to the left most bool in the list.
--
--   This function is unsafe for negative integers and
--   integers bigger than 7.
--   Since it is used internally in this module no error is thrown.
clearBit' :: [Bool] -> Integer -> [Bool]
clearBit' xs n = prefix ++ [False] ++ tail suffix
    where (prefix, suffix) = splitAt n xs
{-# INLINABLE clearBit' #-}

-- | test bit n in a byte represented as a list of bools.
--   This function acts from the left, so zero correponds
--   to the left most bool in the list.
--
--   This function is unsafe for negative integers and
--   integers bigger than 7.
--   Since it is used internally in this module no error is thrown.
testBit' :: [Bool] -> Integer -> Bool
testBit' xs n = xs !! n
{-# INLINABLE testBit' #-}

-- | Set the value of a bit at position n of a 'BuiltinByteString`.
--   Plutus version of `(Data.Bits.setBit)`.
--   This function acts from the left.
--
--   This function will give an error for negative positions and 
--   positions bigger than the size of the 'BuiltinByteString` in bits.
setBit :: BuiltinByteString -> Integer -> BuiltinByteString
setBit bs n
    | n < 0 || n >= 8 * lengthOfByteString bs   = error ()
    | otherwise                                 = takeByteString bytePos bs <>
                                                    consByteString
                                                    (byteBEToInt (setBit' (intToByteBE (indexByteString suffix' 0)) (n `remainder` 8)))
                                                    (dropByteString 1 suffix')
    where bytePos       = n `quotient` 8
          suffix'       = dropByteString bytePos bs
{-# INLINABLE setBit #-}

-- | Clear the value of a bit at position n of a 'BuiltinByteString`.
--   Plutus version of `(Data.Bits.clearBit)`.
--   This function acts from the left.
--
--   This function will give an error for negative positions and 
--   positions bigger than the size of the 'BuiltinByteString` in bits.
clearBit :: BuiltinByteString -> Integer -> BuiltinByteString
clearBit bs n
    | n < 0 || n >= 8 * lengthOfByteString bs   = error ()
    | otherwise                                 = takeByteString bytePos bs <>
                                                    consByteString
                                                    (byteBEToInt (clearBit' (intToByteBE (indexByteString suffix' 0)) (n `remainder` 8)))
                                                    (dropByteString 1 suffix')
    where bytePos       = n `quotient` 8
          suffix'       = dropByteString bytePos bs
{-# INLINABLE clearBit #-}

-- | Test the value of a bit at position n of a `BuiltinBytestring`.
--   Plutus version of `(Data.Bits.testBit)`.
--   This function acts from the left.
--
--   This function will give an error for negative positions and 
--   positions bigger than the size of the 'BuiltinByteString` in bits.
testBit :: BuiltinByteString -> Integer -> Bool
testBit bs n
    | n < 0 || n >= 8 * lengthOfByteString bs   = error ()
    | otherwise                                 = testBit' (intToByteBE (indexByteString bs (n `quotient` 8))) (n `remainder` 8)
{-# INLINABLE testBit #-}