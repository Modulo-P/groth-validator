module Main where

import           Algebra.V1.Test                          as V1
import           Algebra.V2.Test                          as V2
import           Algebra.V3.Test                          as V3
import qualified Data.ByteString                          as BSL
import           Flat                                     (flat)
import           Plutus.V1.Ledger.Api                     (ExCPU (..))
import           Plutus.V2.Ledger.Api                     (ExBudget (..),
                                                           ExMemory (..))
import           PlutusCore                               (DefaultFun,
                                                           DefaultUni,
                                                           NamedDeBruijn)
import qualified PlutusCore                               as PLC
import           UntypedPlutusCore                        (Program)
import qualified UntypedPlutusCore                        as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as Cek

import           BLS12_381_V1.Pairing_bls12381            as BLSV1
import           BLS12_381_V2.Pairing_bls12381            as BLSV2


main :: IO ()
main = do
  putStrLn "InverseP V1"
  printProgramCosts V1.runInversePTest
  BSL.writeFile "compiled/profile/V1/inversePV1.flat" $ flat V1.runInversePTest
  printProgramCosts V2.runInversePTest
  BSL.writeFile "compiled/profile/V2/inversePV2.flat" $ flat V2.runInversePTest
  -- printProgramCosts V2.runSumOfPolys
  BSL.writeFile "compiled/profile/V3/sumOfPolysV3.flat" $ flat V2.runSumOfPolys
  printProgramCosts V3.runInversePTest
  BSL.writeFile "compiled/profile/V3/inversePV3.flat" $ flat V3.runInversePTest
  -- printProgramCosts V3.runSumOfPolys
  BSL.writeFile "compiled/profile/V3/sumOfPolysV3.flat" $ flat V3.runSumOfPolys

mainBLS :: IO ()
mainBLS = do
  printProgramCosts BLSV1.testPlutusBLSV1
  BSL.writeFile "compiled/profile/BLS/blsPairingV1.flat" $ flat BLSV1.testPlutusBLSV1
  printProgramCosts BLSV2.testPlutusBLSV2
  BSL.writeFile "compiled/profile/BLS/blsPairingV2.flat" $ flat BLSV2.testPlutusBLSV2


mainBLSV2 :: IO ()
mainBLSV2 = do
  printProgramCosts BLSV2.testPlutusBLSV2
  BSL.writeFile "compiled/profile/BLS/blsPairingV2.flat" $ flat BLSV2.testPlutusBLSV2

printProgramCosts :: Program NamedDeBruijn DefaultUni DefaultFun () -> IO ()
printProgramCosts prog = do
  let (cpu, mem) = getCostsCek prog
  putStrLn $ "CPU: " ++ "10000000000" ++ " MEM: " ++ "14000000"
  putStrLn $ "CPU: " ++ show cpu ++ " MEM: " ++ show mem

-- | Evaluate a script and return the CPU and memory costs (according to the cost model)
getCostsCek :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun () -> (Integer, Integer)
getCostsCek (UPLC.Program _ _ prog) =
    case Cek.runCekDeBruijn PLC.defaultCekParameters Cek.tallying Cek.noEmitter prog of
      (_res, Cek.TallyingSt _ budget, _logs) ->
          let ExBudget (ExCPU cpu)(ExMemory mem) = budget
          in (toInteger cpu, toInteger mem)

