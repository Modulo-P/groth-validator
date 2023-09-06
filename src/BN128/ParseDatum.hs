{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module BN128.ParseDatum where

import           Data.Aeson
import qualified Data.ByteString.Lazy  as B
import           PlutusTx.Prelude
import           Prelude               (IO, Show (..), String, fail, putStrLn,
                                        read)
import qualified Prelude               as P ((<$>), (<*>), (==))
import qualified System.IO             as S (readFile)
import           Text.Read             (readEither)

import           BN128.ZKPVerification
import           Deploy                (writeJSON)

type FilePath = String


--- SNARKJS' RECORDS ---

-- | Record for snarkjs' verification key
data VerifyZKjson = VerifyZKjson
  { vk_protocol     :: String
  , vk_curve        :: String
  , nPublic         :: Integer
  , vk_alpha_1      :: [String]
  , vk_beta_2       :: [[String]]
  , vk_gamma_2      :: [[String]]
  , vk_delta_2      :: [[String]]
  , vk_alphabeta_12 :: [[[String]]]
  , iC              :: [[String]]
  } deriving Show

instance FromJSON VerifyZKjson where
  parseJSON (Object v) = VerifyZKjson P.<$> v .: "protocol" P.<*> v .: "curve" P.<*> v .: "nPublic" P.<*>
    v .: "vk_alpha_1" P.<*> v .: "vk_beta_2" P.<*> v .: "vk_gamma_2" P.<*> v .: "vk_delta_2" P.<*>
    v .: "vk_alphabeta_12" P.<*> v .: "IC"
  parseJSON _          = fail "verify-ZK JSON parse fail"

-- | Record for snarkjs' proof
data ProofZKjson = ProofZKjson
  { zk_pi_a     :: [String]
  , zk_pi_b     :: [[String]]
  , zk_pi_c     :: [String]
  , zk_protocol :: String
  , zk_curve    :: String
  } deriving Show

instance FromJSON ProofZKjson where
  parseJSON (Object v) = ProofZKjson P.<$> v .: "pi_a" P.<*> v .: "pi_b" P.<*> v .: "pi_c" P.<*>
    v .: "protocol" P.<*> v .: "curve"
  parseJSON _          = fail "proof-ZK JSON parse fail"


--- GENERATE DATUM ---

-- | Generate Datum.  Provide file paths to:
--   1) 'verification.json', 2) 'proof.json', 3) 'public'
generateDatum :: FilePath -> FilePath -> FilePath -> IO (Either String VerifyDatum)
generateDatum verifFile proofFile pubFile = do
  verifInput <- B.readFile verifFile
  proofInput <- B.readFile proofFile
  pubInput   <- S.readFile pubFile
  let pubs = read pubInput :: [String]
  let edatum = do
        proofJSON <- eitherDecode proofInput
        zkproof   <- parseProofJSON proofJSON
        pubinst   <- traverse (readEither @Integer) pubs
        verifJSON <- eitherDecode verifInput
        parseVerifyJSON zkproof pubinst verifJSON
  return (edatum)


--- PRINT ---

proofPrint :: FilePath -> IO ()
proofPrint file = do
  input <- B.readFile file
  case eitherDecode input >>= parseProofJSON of
    Left err  -> putStrLn err
    Right pzk -> putStrLn (show pzk)

-- | Prints Datum.  Need to provide file paths to:
--   1) 'verification.json', 2) 'proof.json', 3) 'public'
verifyDatumPrint :: FilePath -> FilePath -> FilePath -> IO ()
verifyDatumPrint verifFile proofFile pubFile = do
  edatum <- generateDatum verifFile proofFile pubFile
  case edatum of
    Left err  -> putStrLn err
    Right dat -> putStrLn (show dat)


--- DEPLOY ---

-- | Deploys Datum as JSON.  Need to provide file paths to:
--   1) 'verification.json', 2) 'proof.json', 3) 'public', 4) target json file
verifyDatumJSON :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
verifyDatumJSON verifFile proofFile pubFile targetFile = do
  edatum <- generateDatum verifFile proofFile pubFile
  case edatum of
    Left err  -> putStrLn err
    Right dat -> writeJSON targetFile dat


--- PARSERS ---

parseVerifyJSON :: ZKProof -> PublicInst -> VerifyZKjson -> Either String VerifyDatum
parseVerifyJSON zkproof pubinst vzkj = do
  if not $ vk_protocol vzkj P.== "groth16" && vk_curve vzkj P.== "bn128"
    then Left "incorrect protocol or curve"
    else if not $ nPublic vzkj == length pubinst
           then Left "'nPublic' is incompatible with public instance"
           else if not $ (nPublic vzkj) + 1 == length (iC vzkj)
                  then Left "'nPublic' appears incompatible with 'IC'"
                  else return ()
  p_g1Alpha <- parseG1 $ vk_alpha_1 vzkj
  p_g2Beta  <- parseG2 $ vk_beta_2  vzkj
  p_g2Gamma <- parseG2 $ vk_gamma_2 vzkj
  p_g2Delta <- parseG2 $ vk_delta_2 vzkj
  p_g1IC    <- traverse parseG1 $ iC vzkj
  return $ VerifyDatum { g1Alpha = p_g1Alpha
                       , g2Beta  = p_g2Beta
                       , g2Gamma = p_g2Gamma
                       , g2Delta = p_g2Delta
                       , g1IC    = p_g1IC
                       , public  = pubinst
                       , proof   = zkproof
                       }

parseProofJSON :: ProofZKjson -> Either String ZKProof
parseProofJSON pzkj = do
  if not $ zk_protocol pzkj P.== "groth16" && zk_curve pzkj P.== "bn128"
    then Left "incorrect protocol or curve"
    else return ()
  p_g1A <- parseG1 $ zk_pi_a pzkj
  p_g2B <- parseG2 $ zk_pi_b pzkj
  p_g1C <- parseG1 $ zk_pi_c pzkj
  return $ ZKProof { g1A = p_g1A
                   , g2B = p_g2B
                   , g1C = p_g1C
                   }

parseG1 :: [String] -> Either String G1
parseG1 ts
  | length ts /= 3 = Left "expected three projective coordinates"
  | otherwise      = do
      ppt <- traverse (readEither @Integer) ts
      if last ppt == 0
        then Right InftyI
        else if last ppt == 1
               then Right $ (\[x, y, _] -> ECI x y) ppt
               else Left "can't parse point on G1"

parseG2 :: [[String]] -> Either String G2
parseG2 tss
  | length tss /= 3               = Left "expected three projective coordinates"
  | (length <$> tss) /= [2, 2, 2] = Left "expected list of length 2 for polynomial encoding"
  | otherwise       = do
      ppt <- traverse (\ts -> traverse (readEither @Integer) ts) tss
      if last ppt == [0, 0]
        then Right InftyP
        else if last ppt == [1, 0]
               then Right $ (\[[x0, x1], [y0, y1], _] -> ECP (Poly [x0, x1, 0]) (Poly [y0, y1, 0])) ppt
               else Left "can't parse point on G2"

