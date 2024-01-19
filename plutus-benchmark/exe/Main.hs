module Main where

import PlutusBenchmark.BlsField.RunBlsField (runBlsField)
import PlutusBenchmark.Verifier.RunVerifier (runVerifier)
import System.IO (stdout)

main :: IO ()
main = do runVerifier stdout
          runBlsField stdout
