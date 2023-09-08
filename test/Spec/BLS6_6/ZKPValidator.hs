{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Spec.BLS6_6.ZKPValidator where

import           BLS6_6.Test                     (datum3fp)
import           BLS6_6.ZKPVerification          (VerifyDatum (..),
                                                  typedGrothValidator)


import qualified Data.ByteString                 as BSL
import qualified Data.Map                        as Map

import           Cardano.Api.Shelley             (ExecutionUnitPrices (..),
                                                  ProtocolParameters (..))
import           Cardano.Ledger.Alonzo.Scripts   (Prices (..))
import           Cardano.Ledger.Babbage.PParams  (PParams' (PParams), _prices,
                                                  updatePParams)
import           Cardano.Ledger.BaseTypes        (BoundedRational (..))
import           Cardano.Node.Emulator           (PParams,
                                                  Params (emulatorPParams, pNetworkId),
                                                  increaseTransactionLimits',
                                                  pParamsFromProtocolParams)
import           Control.Monad                   (forever, void)
import qualified Control.Monad.Freer.Extras      as Trace
import           Data.Default                    (def)
import           Data.Ratio                      ((%))
import qualified Data.Text                       as T
import qualified Data.Text                       as Text
import           Ledger                          (toPlutusAddress)
import qualified Ledger.Tx.Constraints           as Constraints
import qualified Ledger.Typed.Scripts            as Scripts
import           Plutus.Contract                 (Contract, Endpoint, Promise,
                                                  endpoint, getParams, logInfo,
                                                  selectList,
                                                  submitTxConstraints,
                                                  submitTxConstraintsSpending,
                                                  type (.\/), utxosAt)
import           Plutus.Contract.Test            (w1, w2)
import qualified Plutus.Script.Utils.Ada         as Ada
import qualified Plutus.Trace                    as Trace
import           Plutus.Trace.Emulator           (EmulatorConfig (..))
import           Plutus.Trace.Emulator.Extract   (Command (..),
                                                  ScriptsConfig (..),
                                                  ValidatorMode (..),
                                                  writeScriptsTo)
import           PlutusTx.Prelude                (Integer, Semigroup ((<>)),
                                                  fromMaybe, ($))
import           Prelude                         (IO, (<$>))
import qualified Prelude                         as Haskell
import           Wallet.Emulator.Wallet          (mockWalletAddress)

import           Flat                            (Decoded, flat, unflat)
import           GHC.Real                        (fromIntegral)
import qualified Plutus.Contract.Error
import           Plutus.Script.Utils.V2.Contexts (ScriptContext (..),
                                                  TxInfo (..), TxOut (..),
                                                  TxOutRef (..))
import           Plutus.Script.Utils.Value       (assetClassValue)
import           Plutus.V1.Ledger.Address        (pubKeyHashAddress)
import           Plutus.V1.Ledger.Value          (assetClass)
import           Plutus.V2.Ledger.Api            (PubKeyHash (..),
                                                  ScriptPurpose (..), TxId (..),
                                                  Value, adaSymbol, adaToken,
                                                  always, toData)
import           Plutus.V2.Ledger.Contexts       (TxOut)
import           Plutus.V2.Ledger.Tx             (OutputDatum (..))
import           PlutusCore.Data                 (Data (I))
import           PlutusTx.AssocMap               (empty)
import           PlutusTx.Maybe                  (Maybe (..))
import           PlutusTx.Prelude                (fmap, mempty)


type ValidatorSchema =
  Endpoint "lock" VerifyDatum
  .\/ Endpoint "unlock" VerifyDatum

validatorContract :: Contract
  ()
  ValidatorSchema
  Text.Text
  ()
validatorContract = forever $ selectList [lock, unlock]

lock :: (Plutus.Contract.Error.AsContractError a) => Promise () ValidatorSchema a ()
lock = endpoint @"lock" @VerifyDatum lockFunds

unlock :: (Plutus.Contract.Error.AsContractError a) => Promise () ValidatorSchema a ()
unlock = endpoint @"unlock" @VerifyDatum unlockFunds

lockFunds :: (Plutus.Contract.Error.AsContractError a) => VerifyDatum -> Contract () ValidatorSchema a ()
lockFunds d = do
  let
    tx = Constraints.mustPayToTheScriptWithInlineDatum d (Ada.lovelaceValueOf 10_000_000)
  void $ submitTxConstraints typedGrothValidator tx

unlockFunds :: (Plutus.Contract.Error.AsContractError a) => VerifyDatum -> Contract () ValidatorSchema a ()
unlockFunds _ = do
  networkId <- pNetworkId <$> getParams
  let contractAddress = Scripts.validatorCardanoAddress networkId typedGrothValidator
  utxos <- utxosAt contractAddress

  let
    orefs = Haskell.fst $ Haskell.head $ Map.toList utxos
  logInfo @Haskell.String $ "OREF: " Haskell.++ Haskell.show  orefs
  let
    tx = Constraints.mustSpendOutputFromTheScript orefs ()
      <> Constraints.mustPayToAddress (toPlutusAddress $ mockWalletAddress w2) (Ada.lovelaceValueOf 10_000_000)
  logInfo @Haskell.String "Hello World"
  void $ submitTxConstraintsSpending typedGrothValidator utxos tx



lockTrace:: Trace.EmulatorTrace ()
lockTrace = do
  h1 <- Trace.activateContractWallet w1 validatorContract
  h2 <- Trace.activateContractWallet w2 validatorContract
  Trace.callEndpoint @"lock" h1 datum3fp
  void $ Trace.waitNSlots 10
  Trace.logInfo @Haskell.String "Starting unlock"
  Trace.callEndpoint @"unlock" h2 datum3fp
  void $ Trace.waitNSlots 10
  Trace.logInfo @Haskell.String "Unlocking complete"
  void $ Trace.waitNSlots 10
  void Trace.nextSlot


runLockTrace :: IO ()
runLockTrace =
  Trace.runEmulatorTraceIO' def (def {_params = increaseLimits defaultParams}) lockTrace

increaseLimits :: Params -> Params
increaseLimits = increaseTransactionLimits' 1_000_000 1_000_000 1_000_000


defaultParams :: Params
defaultParams = def {
  emulatorPParams = defaultPParams
}

defaultPParams :: PParams
defaultPParams =  pParamsFromProtocolParams (def { protocolParamPrices = Haskell.Just (ExecutionUnitPrices {priceExecutionSteps = 0, priceExecutionMemory = 0})
})

datum3fpAsData :: Data
datum3fpAsData = toData datum3fp

flattedDatum3fpAsData :: BSL.ByteString
flattedDatum3fpAsData = flat datum3fpAsData

writeDatum3fpFlat :: IO ()
writeDatum3fpFlat = do
  BSL.writeFile "compiled/profile/datum-3fp.flat" flattedDatum3fpAsData

writeUnitAsFlat :: IO ()
writeUnitAsFlat = do
  BSL.writeFile "compiled/profile/unit.flat" (flat (toData ()))

-- | A very crude deterministic generator for 'ScriptContext's with size
-- approximately proportional to the input integer.
mkScriptContext :: Integer -> ScriptContext
mkScriptContext i = ScriptContext (mkTxInfo i) (Spending (TxOutRef (TxId "") 0))

mkTxInfo :: Integer -> TxInfo
mkTxInfo i = TxInfo {
  txInfoInputs=mempty,
  txInfoReferenceInputs=mempty,
  txInfoOutputs=fmap mkTxOut [1..i],
  txInfoFee=mempty,
  txInfoMint=mempty,
  txInfoWdrl=empty,
  txInfoValidRange=always,
  txInfoSignatories=mempty,
  txInfoRedeemers=empty,
  txInfoData=empty,
  txInfoId=TxId "",
  txInfoDCert=mempty
  }

mkTxOut :: Integer -> TxOut
mkTxOut i = TxOut {
  txOutAddress=pubKeyHashAddress (PubKeyHash ""),
  txOutValue=mkValue i,
  txOutDatum=NoOutputDatum,
  txOutReferenceScript=Nothing
  }

mkValue :: Integer -> Value
mkValue = assetClassValue (assetClass adaSymbol adaToken)

writeScriptContextAsFlat :: IO ()
writeScriptContextAsFlat = do
  BSL.writeFile "compiled/profile/script-context.flat" (flat (toData (mkScriptContext 1)))

writeOneAsFlat :: IO ()
writeOneAsFlat = do
  BSL.writeFile "compiled/profile/one.flat" (flat (PlutusCore.Data.I 1))
