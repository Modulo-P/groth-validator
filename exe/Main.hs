module Main where

import           Algebra.V1.Test                          as V1
import           Algebra.V2.Test                          as V2
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


main :: IO ()
main = do
  putStrLn "InverseP V1"
  printProgramCosts V1.runInversePTest
  BSL.writeFile "compiled/profile/V1/inversePV1.flat" $ flat V1.runInversePTest
  printProgramCosts V2.runInversePTest
  BSL.writeFile "compiled/profile/V2/inversePV2.flat" $ flat V2.runInversePTest


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

