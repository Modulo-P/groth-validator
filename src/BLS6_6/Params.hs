{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module BLS6_6.Params where

import           PlutusTx.Prelude


-----------------------------------------------------------------------
--------------   Parameters for Elliptic Curve BLS6_6   ---------------
-----------------------------------------------------------------------


----- EMBEDDING DEGREE -----

{-# INLINABLE k0 #-}
k0 :: Integer
k0 = 6


----- FIELD ORDER -----

{-# INLINABLE q0 #-}
q0 :: Integer
q0 = 43


----- PAIRING GROUP ORDER -----

{-# INLINABLE rEC #-}
rEC   :: Integer     -- Group order
rEC  = 13

{-# INLINABLE bitsR #-}
bitsR  :: [Integer]   -- Binary representation of r_EC
bitsR = [1, 0, 1, 1]

-- Even/Odd sign of group order

{-# INLINABLE rSign #-}
rSign :: Integer  -- (-1)^r_EC
rSign = -1


----- IRREDUCIBLE POLYNOMIAL -----

{-# INLINABLE poly0 #-}
poly0 :: [Integer]
poly0 = [6, 0, 0, 0, 0, 0, 1]  -- 6 + x^6
