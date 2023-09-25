{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Algebra.V1.Polynomials where

import           BLS6_6.Params       (k0, poly0)

import           Data.Aeson          (FromJSON, ToJSON)

import           Algebra.V1.Integers (inverseI, (\*))
import           Algebra.V1.Types    (EuclideanRing (..), Field (..))
import           GHC.Generics        (Generic)
import           PlutusTx.Prelude    (AdditiveGroup (..), AdditiveMonoid (..),
                                      AdditiveSemigroup (..), Bool, Eq (..),
                                      Integer, MultiplicativeMonoid (..),
                                      MultiplicativeSemigroup (..), Ord ((<)),
                                      Semigroup (..), head, length, otherwise,
                                      reverse, snd, sum, take, zipWith, (!!),
                                      ($), (&&), (.), (<$>), (<=))
import           Prelude             (Show)



-- TODO: Remove this once we have a better way to do this
-- Helper functions --

{-# INLINABLE replicate #-}
replicate :: Integer -> Integer -> [Integer]
replicate !a !b = if a <= 0 then [] else b : replicate (a-1) b

{-# INLINABLE last #-}
last :: [a] -> a
last !xs = xs !! (length xs - 1)

{-# INLINABLE range #-}
range :: Integer -> [Integer]
range !n = go [n]
  where
    go ((!x):(!xs)) = if x <= 1 then x:xs else go ((x - 1):x:xs)
    go _            = []


-- Polynomials --

-- | The "polynomials"
newtype Poly = Poly [Integer]
  deriving stock (Show, Generic)
  deriving newtype (Eq)
  deriving anyclass (ToJSON, FromJSON)

instance AdditiveSemigroup Poly where
  {-# INLINABLE (+) #-}
  (+) :: Poly -> Poly -> Poly
  (+) (Poly !ps) (Poly !qs) = Poly $ zipWith (+) ps qs

instance AdditiveMonoid Poly where
  {-# INLINABLE zero #-}
  zero :: Poly
  zero = Poly $ replicate (k0 + 1) 0

instance AdditiveGroup Poly where
  {-# INLINABLE (-) #-}
  (-) :: Poly -> Poly -> Poly
  (-) (Poly !ps) (Poly !qs) = Poly $ zipWith (-) ps qs

instance MultiplicativeSemigroup Poly where
  {-# INLINABLE (*) #-}
  (*) :: Poly -> Poly -> Poly
  (*) (Poly !ps) (Poly !qs) = Poly $ term ps qs <$> range (length ps)
    where
      term :: [Integer] -> [Integer] -> Integer -> Integer
      term !ps' !qs' !n = sum $ zipWith (*) (take n ps') (reverse $ take n qs')

instance MultiplicativeMonoid Poly where
  {-# INLINABLE one #-}
  one :: Poly
  one = Poly $ [1] <> replicate k0 0

-- | "fit" adjusts 'poly' to a list of length 'k0 + 1'
{-# INLINABLE fit #-}
fit :: Poly -> Poly
fit (Poly !ps) = Poly $ ps <> replicate (k0 - length ps + 1) 0

-- | Keep numbers bounded
-- bound :: Poly -> Poly
-- bound (Poly ps) = Poly (mod0 <$> ps)

-- | 'degree' gives the degree of 'poly' in field of "integers mod q0"
{-# INLINABLE degree #-}
degree :: Poly -> Integer
degree (Poly [_]) = 0
degree (Poly !ps) = if mod0 (last ps) == 0
  then degree $ Poly (take (length ps - 1) ps)
  else length ps - 1

-- | Note that 'divModE' and 'euclides' can only be applied to "equal-length"
-- polynomials
instance EuclideanRing Poly where
  {-# INLINABLE divModE #-}
  divModE :: Poly -> Poly -> (Poly, Poly)
  divModE n@(Poly !ns) !d = go (d, zero, n)
    where
      len = length ns
      go :: (Poly, Poly, Poly) -> (Poly, Poly)
      go (d'@(Poly !ds), !q, r@(Poly !rs))
        | deg_r == 0 && mod0 (head rs) == 0 = (q, zero)
        | deg_r < deg_d         = (q, r)
        | otherwise                             = let
               !t1 = replicate (deg_r - deg_d) 0
               !r2 = rs !! deg_r
               !d2 = ds !! deg_d
               !t2 = [r2 \* inverseI d2]
               !t3 = replicate (len - deg_r + deg_d - 1) 0
               !t  = Poly $ t1 <> t2 <> t3
            in
               go (d', q + t, r - t * d)
        where
          !deg_d = degree d
          !deg_r = degree r

  {-# INLINABLE mod #-}
  mod :: Poly -> Poly -> Poly
  mod !x !y = snd $ divModE x y


-- | Euclides algorithm for polynomials
{-# INLINABLE euclidesP #-}
euclidesP :: Poly -> Poly -> (Poly, Poly, Poly)
euclidesP !x !y = (Poly $ (i0 \*) <$> rs, Poly $ (i0 \*) <$> ss, Poly $ (i0 \*) <$> ts)
  where
    !i0 = inverseI . head $ rs
    !(Poly !rs, Poly !ss, Poly !ts) = go (x, one, zero) (y, zero, one)
    go :: (Poly, Poly, Poly) -> (Poly, Poly, Poly) -> (Poly, Poly, Poly)
    go (!r0, !s0, !t0) (!r1, !s1, !t1)
      | r1 == zero = (r0, s0, t0)
      | otherwise  = let
            !(!q, !r2) = divModE r0 r1
            !s2      = s0 - q * s1
            !t2      = t0 - q * t1
          in
            go (r1, s1, t1) (r2, s2, t2)


instance Field Poly where
  {-# INLINABLE mod0 #-}
  mod0 :: Poly -> Poly
  mod0 !p  = Poly $ mod0 <$> ps
    where
      Poly !ps = mod p (Poly poly0)

  {-# INLINABLE (===) #-}
  (===) :: Poly -> Poly -> Bool
  (===) !p !q = mod (p - q) (Poly poly0) == zero


-- | Embed base field into extended field
{-# INLINABLE embed #-}
embed :: Integer -> Poly
embed !n = fit $ Poly [n]

-- | Multiplication in extended finite field.  Note that 'divModE' and 'euclides' can only
-- be applied to "equal-length" polynomials.
{-# INLINABLE (\*\) #-}
(\*\) :: Poly -> Poly -> Poly
(\*\) (Poly !xs) (Poly !ys) = Poly $ mod0 <$> zs
  where
    !tailZeroes = replicate k0 0
    !x = Poly $ xs <> tailZeroes
    !y = Poly $ ys <> tailZeroes
    !o = Poly $ poly0 <> tailZeroes
    Poly !zs' = mod (x * y) o
    !zs = take (k0 + 1) zs'

-- | Inverse operator in extended finite field
{-# INLINABLE inverseP #-}
inverseP :: Poly -> Poly
inverseP !x = mod t p0
  where
    !p0 = Poly poly0
    !(_, _, !t) = euclidesP p0 x
