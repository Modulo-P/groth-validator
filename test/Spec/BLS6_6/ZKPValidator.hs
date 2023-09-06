{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Spec.BLS6_6.ZKPValidator (runLockTrace) where

import           BLS6_6.Test                    (datum3fp)
import           BLS6_6.ZKPVerification         (VerifyDatum (..),
                                                 typedGrothValidator)



import qualified Data.Map                       as Map

import           Cardano.Api.Shelley            (ExecutionUnitPrices (..),
                                                 ProtocolParameters (..))
import           Cardano.Ledger.Alonzo.Scripts  (Prices (..))
import           Cardano.Ledger.Babbage.PParams (PParams' (PParams), _prices,
                                                 updatePParams)
import           Cardano.Ledger.BaseTypes       (BoundedRational (..))
import           Cardano.Node.Emulator          (PParams,
                                                 Params (emulatorPParams, pNetworkId),
                                                 increaseTransactionLimits',
                                                 pParamsFromProtocolParams)
import           Control.Monad                  (forever, void)
import qualified Control.Monad.Freer.Extras     as Trace
import           Data.Default                   (def)
import           Data.Ratio                     ((%))
import qualified Data.Text                      as T
import qualified Data.Text                      as Text
import           Ledger                         (toPlutusAddress)
import qualified Ledger.Tx.Constraints          as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
import           Plutus.Contract                (Contract, Endpoint, Promise,
                                                 endpoint, getParams, logInfo,
                                                 selectList,
                                                 submitTxConstraints,
                                                 submitTxConstraintsSpending,
                                                 type (.\/), utxosAt)
import           Plutus.Contract.Test           (w1, w2)
import qualified Plutus.Script.Utils.Ada        as Ada
import qualified Plutus.Trace                   as Trace
import           Plutus.Trace.Emulator          (EmulatorConfig (..))
import           PlutusTx.Prelude               (Semigroup ((<>)), fromMaybe,
                                                 ($))
import           Prelude                        (IO, (<$>))
import qualified Prelude                        as Haskell
import           Wallet.Emulator.Wallet         (mockWalletAddress)


type ValidatorSchema =
  Endpoint "lock" VerifyDatum
  .\/ Endpoint "unlock" VerifyDatum

validatorContract :: Contract
  ()
  ValidatorSchema
  Text.Text
  ()
validatorContract = forever $ selectList [lock, unlock]

lock :: Promise () ValidatorSchema T.Text ()
lock = endpoint @"lock" @VerifyDatum lockFunds

unlock :: Promise () ValidatorSchema T.Text ()
unlock = endpoint @"unlock" @VerifyDatum unlockFunds

lockFunds :: VerifyDatum -> Contract () ValidatorSchema T.Text ()
lockFunds d = do
  let
    tx = Constraints.mustPayToTheScriptWithInlineDatum d (Ada.lovelaceValueOf 10_000_000)
  void $ submitTxConstraints typedGrothValidator tx

unlockFunds ::  VerifyDatum -> Contract () ValidatorSchema T.Text ()
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
