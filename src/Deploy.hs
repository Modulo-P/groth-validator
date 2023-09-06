{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Deploy
    ( writeJSON
    , writeValidator
    , writePolicy
    , writePolicy'
    , writeCS
    , writeUnit
    , writeTokenName
    ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.Text.IO          as TIO
import           Text.Hex
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Plutus.V2.Ledger.Api as V2L
import qualified Plutus.Script.Utils.V2.Scripts as Scripts

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeUnit :: FilePath -> IO ()
writeUnit file = writeJSON file ()

writeValidator :: FilePath -> Scripts.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . V2L.unValidatorScript

writePolicy :: FilePath -> Scripts.MintingPolicy -> IO (Either (FileError ()) ())
writePolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . V2L.unMintingPolicyScript

writeCS :: FilePath -> V2L.CurrencySymbol -> IO ()
writeCS file cs = writeFile file (show cs)

type FilePathWithoutExt = String

-- | Please omit file extension.  Generates .plutus & .policy files (script & currency symbol).
writePolicy' :: FilePathWithoutExt -> Scripts.MintingPolicy -> IO ()
writePolicy' file mp = do
  let fileP = file <> ".plutus"
      fileH = file <> ".policy"
  _ <- writeFileTextEnvelope @(PlutusScript PlutusScriptV2) fileP Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . V2L.unMintingPolicyScript $ mp
  writeFile fileH (show $ Scripts.scriptCurrencySymbol mp)

writeTokenName :: FilePath -> V2L.TokenName -> IO ()
writeTokenName file tn = do
  let hex = encodeHex . V2L.fromBuiltin . V2L.unTokenName $ tn
  TIO.writeFile file hex
