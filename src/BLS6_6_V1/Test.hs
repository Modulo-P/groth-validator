-- {-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE TypeApplications           #-}
-- {-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- {-# OPTIONS_GHC -Wno-name-shadowing      #-}

module BLS6_6_V1.Test where

import           PlutusTx.Prelude

import           BLS6_6_V1.ZKPVerification

proof3fp :: ZKProof
proof3fp = ZKProof
  { g1A = ECI 35 15
  , g2B = ECP (fit $ Poly [0,0,7]) (fit $ Poly [0,0,0,27])
  , g1C = ECI 13 28
  }

datum3fp :: VerifyDatum
datum3fp = VerifyDatum
  { g1Alpha = ECI 27 34
  , g2Beta  = ECP (fit $ Poly [0, 0, 16]) (fit $ Poly [0, 0, 0, 28])
  , g2Gamma = ECP (fit $ Poly [0, 0, 37]) (fit $ Poly [0, 0, 0, 27])
  , g2Delta = ECP (fit $ Poly [0, 0, 42]) (fit $ Poly [0, 0, 0, 16])
  , g1IC    = [InftyI, ECI 33 9]
  , public  = [11]
  , proof   = proof3fp
  }


--- Testing Functions ---

onG1 :: G1 -> Bool
onG1 InftyI    = True
onG1 (ECI x y) = y \* y == x \* x \* x + 6

onG2 :: G2 -> Bool
onG2 InftyP    = True
onG2 (ECP x y) = y \*\ y === x \*\ x \*\ x + embed 6


--- Test Validator ---

testValidator1 :: Bool
testValidator1 = mkVerificationValidator' datum3fp ()
