module Main where
import           Spec.BLS6_6.ZKPValidator    (runLockTrace)
import qualified Spec.BLS6_6_V1.ZKPValidator as V1 (runLockTrace)


main :: IO ()
main = runLockTrace

runV1 :: IO ()
runV1 = V1.runLockTrace
