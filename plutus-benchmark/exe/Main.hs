module Main where

import PlutusBenchmark.Verifier.RunVerifier (runVerifier)
import PlutusBenchmark.BlsField.RunBlsField (runBlsField)
import System.IO (stdout)

main :: IO ()
main = do runVerifier stdout
          runBlsField stdout