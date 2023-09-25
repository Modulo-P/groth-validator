{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE NoImplicitPrelude  #-}


module Algebra.V2.Integers where
import           PlutusTx.Builtins (modInteger)
import           PlutusTx.Prelude  (AdditiveGroup (..), Bool, Eq (..), Integer,
                                    MultiplicativeSemigroup (..), Ord ((<)),
                                    divMod, negate, otherwise, traceError, ($))


import           Algebra.V2.Types  (EuclideanRing (..), Field (..))
import           BLS6_6.Params
import           Prelude           ()


-- Integers --

instance EuclideanRing Integer where
  {-# INLINABLE divModE #-}
  divModE :: Integer -> Integer -> (Integer, Integer)
  divModE !a !b = divMod a b

  {-# INLINABLE mod #-}
  mod :: Integer -> Integer -> Integer
  mod !a !b    = modInteger a b

{-# INLINABLE eculidesI #-}
eculidesI :: Integer -> Integer -> (Integer, Integer, Integer)
eculidesI !x !y = if r < 0 then (negate r, negate s, negate t) else (r, s, t)
  where
    !(!r, !s, !t) = go (x, 1, 0) (y, 0, 1)
    go (!r0, !s0, !t0) (!r1, !s1, !t1)
      | r1 == 0   = (r0, s0, t0)
      | otherwise = let
            !(!q, !r2) = divModE r0 r1
            !s2      = s0 - q * s1
            !t2      = t0 - q * t1
          in
            go (r1, s1, t1) (r2, s2, t2)

instance Field Integer where
  {-# INLINABLE mod0 #-}
  mod0 :: Integer -> Integer
  mod0 !n = mod n q0

  {-# INLINABLE (===) #-}
  (===) :: Integer -> Integer -> Bool
  (===) !n !m = mod0 (n - m) == 0


-- | Multiplication in base finite field
{-# INLINABLE (\*) #-}
(\*) :: Integer -> Integer -> Integer
(\*) !n !m  = mod0 $ n * m

-- | Inverse operator in base finite field
{-# INLINABLE inverseI #-}
inverseI :: Integer -> Integer
inverseI !n = if mod0 n == 0
  then traceError "tried to divide by zero"
  else mod0 t
      where
        !(_, _, !t) = eculidesI q0 n
