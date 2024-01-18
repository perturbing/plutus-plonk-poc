{-# LANGUAGE OverloadedStrings #-}

module PlutusBenchmark.BlsField.RunBlsField
( runBlsField
) where

import PlutusBenchmark.BlsField.Scripts (blsFieldAddScalarsScript, blsFieldMulScalarsScript 
        , listOfSizedByteStrings, modularExponentiationScalarScript, modExpPow2Script, invertScalarsScript)
import PlutusTx.Prelude ( toBuiltin )
import PlutusTx.Builtins (byteStringToInteger)
import Plutus.Crypto.BlsField (mkScalar)

import PlutusBenchmark.Common ( printHeader, printSizeStatistics, TestSize(TestSize) )

import System.IO (Handle)
import Text.Printf (hPrintf)

printCostsBlsAddScalarsField :: Handle -> Integer -> IO ()
printCostsBlsAddScalarsField h n =
    let script = blsFieldAddScalarsScript . map (mkScalar . byteStringToInteger False . toBuiltin) $ listOfSizedByteStrings n 31
    in printSizeStatistics h (TestSize n) script

printCostsBlsMulScalarsField :: Handle -> Integer -> IO ()
printCostsBlsMulScalarsField h n =
    let script = blsFieldMulScalarsScript . map (mkScalar . byteStringToInteger False . toBuiltin) $ listOfSizedByteStrings n 31
    in printSizeStatistics h (TestSize n) script

printCostsModExpScalar :: Handle -> Integer -> IO ()
printCostsModExpScalar h n =
    let script = modularExponentiationScalarScript (map (mkScalar . byteStringToInteger False . toBuiltin) $ listOfSizedByteStrings n 31) 52435875175126190479447740508185965837690552500527637822603658699938581184510
    in printSizeStatistics h (TestSize n) script

printCostsModExpScalar2 :: Handle -> Integer -> IO ()
printCostsModExpScalar2 h n =
    let script = modExpPow2Script (mkScalar 52435875175126190479447740508185965837690552500527637822603658699938581184510) n
    in printSizeStatistics h (TestSize n) script

printCostsBlsInvertAndAdd :: Handle -> Integer -> IO ()
printCostsBlsInvertAndAdd h n =
    let script = invertScalarsScript . map (mkScalar . byteStringToInteger False . toBuiltin) $ listOfSizedByteStrings n 31
    in printSizeStatistics h (TestSize n) script

runBlsField :: Handle -> IO ()
runBlsField h = do
    hPrintf h "\n\n"

    hPrintf h "n scalars additions (size 31 bytes)\n\n"
    printHeader h
    mapM_ (printCostsBlsAddScalarsField h) [0, 400..4000]
    hPrintf h "\n\n"

    hPrintf h "n scalars multiplications (size 31 bytes)\n\n"
    printHeader h
    mapM_ (printCostsBlsMulScalarsField h) [0, 400..4000]
    hPrintf h "\n\n"

    hPrintf h "n scalar exponentiation with exponent of size 32 bytes \n\n"
    printHeader h
    mapM_ (printCostsModExpScalar h) [0..8]
    hPrintf h "\n\n"

    hPrintf h "scalar exponent for a^e if e = 2^n \n\n"
    printHeader h
    mapM_ (printCostsModExpScalar2 h) [0, 4..32]
    hPrintf h "\n\n"

    hPrintf h "n scalar inversion \n\n"
    printHeader h
    mapM_ (printCostsBlsInvertAndAdd h) [0, 1..8]
    hPrintf h "\n\n"