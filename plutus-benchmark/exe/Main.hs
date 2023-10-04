module Main where

import PlutusBenchmark.Verifier.RunTests (runVerifier)
import System.IO (stdout)

main :: IO ()
main = runVerifier stdout