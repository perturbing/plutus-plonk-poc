{-# LANGUAGE OverloadedStrings #-}

module PlutusBenchmark.BlsField.RunBlsField
( runBlsField
) where

import PlutusBenchmark.BlsField.Scripts (blsFieldAddScalarsScript, blsFieldMulScalarsScript, listOfSizedByteStrings, modularExponentiationScalarScript, modularExponentiationScalarScript2)
import PlutusTx.Prelude (byteStringToInteger, toBuiltin)
import Plutus.Crypto.BlsField (mkScalar)

import PlutusBenchmark.Common ( printHeader, printSizeStatistics, TestSize(TestSize) )

import System.IO (Handle)
import Text.Printf (hPrintf)

printCostsBlsAddScalarsField :: Handle -> Integer -> IO ()
printCostsBlsAddScalarsField h n =
    let script = blsFieldAddScalarsScript . map (mkScalar . byteStringToInteger . toBuiltin) $ listOfSizedByteStrings n 31
    in printSizeStatistics h (TestSize n) script

printCostsBlsMulScalarsField :: Handle -> Integer -> IO ()
printCostsBlsMulScalarsField h n =
    let script = blsFieldMulScalarsScript . map (mkScalar . byteStringToInteger . toBuiltin) $ listOfSizedByteStrings n 31
    in printSizeStatistics h (TestSize n) script

printCostsModExpScalar :: Handle -> Integer -> IO ()
printCostsModExpScalar h n =
    let script = modularExponentiationScalarScript (mkScalar 52435875175126190479447740508185965837690552500527637822603658699938581184510) $ toBuiltin (head (listOfSizedByteStrings 1 n))
    in printSizeStatistics h (TestSize n) script

printCostsModExpScalar2 :: Handle -> Integer -> IO ()
printCostsModExpScalar2 h n =
    let script = modularExponentiationScalarScript2 (mkScalar 52435875175126190479447740508185965837690552500527637822603658699938581184510) n
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

    hPrintf h "mod exp scalar with shifts\n\n"
    printHeader h
    mapM_ (printCostsModExpScalar h) [0, 4..32]
    hPrintf h "\n\n"

    hPrintf h "mod exp scalar with full byte shifts\n\n"
    printHeader h
    mapM_ (printCostsModExpScalar2 h) [0, 4..32]
    hPrintf h "\n\n"