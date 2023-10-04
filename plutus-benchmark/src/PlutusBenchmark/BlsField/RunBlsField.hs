module PlutusBenchmark.BlsField.RunBlsField
( runBlsField
) where

import PlutusBenchmark.BlsField.Scripts (blsFieldAddScalarsScript, blsFieldMulScalarsScript, listOfSizedByteStrings)
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
