{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BN128.Test where

import           PlutusTx.Prelude
import           Prelude               (IO, print, putStrLn)

import           BN128.ParseDatum      (FilePath, generateDatum)
import           BN128.ZKPVerification (mkVerificationValidator')


--- TEST 'mkVerificationValidator' ---

-- | Need to provide file paths to:
--   1) 'verification.json', 2) 'proof.json', 3) 'public'
testValidator2 :: FilePath -> FilePath -> FilePath -> IO ()
testValidator2 verifFile proofFile pubFile = do
  edatum <- generateDatum verifFile proofFile pubFile
  case edatum of
    Left err  -> putStrLn err
    Right dat -> print $ mkVerificationValidator' dat ()

-- Directory '../../snarkjs/' contains snarkjs-output-files that can be used with 'testValidator2'.
