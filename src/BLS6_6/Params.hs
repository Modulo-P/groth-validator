{-# LANGUAGE NoImplicitPrelude #-}

module BLS6_6.Params where

import PlutusTx.Prelude


-----------------------------------------------------------------------
--------------   Parameters for Elliptic Curve BLS6_6   ---------------
-----------------------------------------------------------------------


----- EMBEDDING DEGREE -----

k0 :: Integer
k0 = 6


----- FIELD ORDER -----

q0 :: Integer
q0 = 43


----- PAIRING GROUP ORDER -----

r_EC   :: Integer     -- Group order
r_EC  = 13

bitsR  :: [Integer]   -- Binary representation of r_EC
bitsR = [1, 0, 1, 1]

-- Even/Odd sign of group order

r_sign :: Integer  -- (-1)^r_EC
r_sign = -1


----- IRREDUCIBLE POLYNOMIAL -----

poly0 :: [Integer]
poly0 = [6, 0, 0, 0, 0, 0, 1]  -- 6 + x^6
