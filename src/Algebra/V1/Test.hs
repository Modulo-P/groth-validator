{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}


-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}


module Algebra.V1.Test where

import           Algebra.V1.Polynomials

import           PlutusCore             (DefaultUni)
import           PlutusCore.Default     (DefaultFun)
import           PlutusTx               as Tx
import qualified UntypedPlutusCore      as UPLC



poly01 :: Poly
poly01 = Poly [0, 1, 2, 3, 4, 5, 6]

poly02 :: Poly
poly02 = Poly [6, 5, 4, 3, 2, 1, 0]

runInversePTest :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
runInversePTest = getPlc $$(Tx.compile [|| inverseP poly01 ||])
