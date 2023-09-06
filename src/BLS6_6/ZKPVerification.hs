{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing      #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE StrictData                 #-}

module BLS6_6.ZKPVerification where

import           PlutusTx
import           PlutusTx.Builtins                    (modInteger)
import           PlutusTx.Prelude
import           Prelude                              (Show)

import qualified Plutus.V2.Ledger.Api                 as PlutusV2

import           BLS6_6.Params
import           Data.Aeson                           (FromJSON, ToJSON)
import           GHC.Generics                         (Generic)
import qualified Ledger.Typed.Scripts                 as PlutusV2
import qualified Plutus.Script.Utils.V2.Typed.Scripts as V2


-----------------------------------------------------------------------
-----------------------------   Algebra   -----------------------------
-----------------------------------------------------------------------

infix  4 ===
infixl 7 \*
infixl 7 \*\


-- Helper functions --

replicate :: Integer -> a -> [a]
replicate a b = if a <= 0 then [] else b : (replicate (a-1) b)

last :: [a] -> a
last xs = xs !! (length xs - 1)

range :: Integer -> [Integer]
range n = go [n]
  where
    go ((!x):(!xs)) = if x <= 1 then x:xs else go ((x - 1):x:xs)


-- Class Declarations --

-- | The 'EuclideanRing' class
class Ring a => EuclideanRing a where
  divModE  :: a -> a -> (a, a)
  mod      :: a -> a -> a


-- | The 'Field' class
class (Eq a, Ring a) => Field a where
  mod0    :: a -> a
  (===)   :: a -> a -> Bool  -- Congruence


-- Integers --

instance EuclideanRing Integer where
  divModE = divMod

  mod     = modInteger

{-# INLINABLE euclides_I #-}
euclides_I :: Integer -> Integer -> (Integer, Integer, Integer)
euclides_I x y = if r < 0 then (negate r, negate s, negate t) else (r, s, t)
  where
    (r, s, t) = go (x, 1, 0) (y, 0, 1)
    go (!r0, !s0, !t0) (!r1, !s1, !t1)
      | r1 == 0   = (r0, s0, t0)
      | otherwise = let
             (q, r2) = divModE r0 r1
             s2      = s0 - q * s1
             t2      = t0 - q * t1
          in
             go (r1, s1, t1) (r2, s2, t2)

instance Field Integer where
  mod0 n = mod n q0

  (===) n m = mod0 (n - m) == 0


-- | Multiplication in base finite field
{-# INLINABLE (\*) #-}
(\*) :: Integer -> Integer -> Integer
(\*) n m  = mod0 $ n * m

-- | Inverse operator in base finite field
{-# INLINABLE inverse_I #-}
inverse_I :: Integer -> Integer
inverse_I n = if mod0 n == 0
  then traceError "tried to divide by zero"
  else mod0 t
       where
         (_, _, t) = euclides_I q0 n


-- Polynomials --

-- | The "polynomials"
newtype Poly = Poly [Integer]
  deriving stock (Show, Generic)
  deriving newtype (Eq)
  deriving anyclass (ToJSON, FromJSON)

instance AdditiveSemigroup Poly where
  (+) (Poly ps) (Poly qs) = Poly $ zipWith (+) ps qs

instance AdditiveMonoid Poly where
  zero = Poly $ replicate (k0 + 1) 0

instance AdditiveGroup Poly where
  (-) (Poly ps) (Poly qs) = Poly $ zipWith (-) ps qs

instance MultiplicativeSemigroup Poly where
  (*) (Poly ps) (Poly qs) = Poly $ term ps qs <$> range (length ps)
    where
      term :: [Integer] -> [Integer] -> Integer -> Integer
      term ps qs n = sum $ zipWith (*) (take n ps) (reverse $ take n qs)

instance MultiplicativeMonoid Poly where
  one = Poly $ [1] <> (replicate k0 0)

-- | "fit" adjusts 'poly' to a list of length 'k0 + 1'
fit :: Poly -> Poly
fit (Poly ps) = Poly $ ps <> (replicate (k0 - length ps + 1) 0)

-- | Keep numbers bounded
-- bound :: Poly -> Poly
-- bound (Poly ps) = Poly (mod0 <$> ps)

-- | 'degree' gives the degree of 'poly' in field of "integers mod q0"
{-# INLINABLE degree #-}
degree :: Poly -> Integer
degree (Poly [_]) = 0
degree (Poly ps) = if mod0 (last ps) == 0
  then degree $ Poly (take (length ps - 1) ps)
  else length ps - 1

-- | Note that 'divModE' and 'euclides' can only be applied to "equal-length"
-- polynomials
instance EuclideanRing Poly where
  {-# INLINABLE divModE #-}
  divModE n@(Poly ns) d = go (d, zero, n)
    where
      len = length ns
      go :: (Poly, Poly, Poly) -> (Poly, Poly)
      go (d@(Poly !ds), !q, r@(Poly !rs))
        | deg_r == 0 && (mod0 $ head rs) == 0 = (q, zero)
        | deg_r < deg_d         = (q, r)
        | otherwise                             = let
               t1 = replicate (deg_r - deg_d) 0
               r2 = rs !! deg_r
               d2 = ds !! deg_d
               t2 = [r2 \* (inverse_I d2)]
               t3 = replicate (len - deg_r + deg_d - 1) 0
               t  = Poly $ t1 <> t2 <> t3
            in
               go (d, q + t, r - t * d)
        where
          deg_d = degree d
          deg_r = degree r

  mod x y = snd $ divModE x y


-- | Euclides algorithm for polynomials
{-# INLINABLE euclides_P #-}
euclides_P :: Poly -> Poly -> (Poly, Poly, Poly)
euclides_P x y = (Poly $ (i0 \*) <$> rs, Poly $ (i0 \*) <$> ss, Poly $ (i0 \*) <$> ts)
  where
    i0 = inverse_I . head $ rs
    (Poly rs, Poly ss, Poly ts) = go (x, one, zero) (y, zero, one)
    go :: (Poly, Poly, Poly) -> (Poly, Poly, Poly) -> (Poly, Poly, Poly)
    go (!r0, !s0, !t0) (!r1, !s1, !t1)
      | r1 == zero = (r0, s0, t0)
      | otherwise  = let
            (q, r2) = divModE r0 r1
            s2      = s0 - q * s1
            t2      = t0 - q * t1
          in
            go (r1, s1, t1) (r2, s2, t2)


instance Field Poly where
  mod0 p  = Poly $ mod0 <$> ps
    where
      Poly ps = mod p (Poly poly0)

  (===) p q = mod (p - q) (Poly poly0) == zero


-- | Embed base field into extended field
{-# INLINABLE embed #-}
embed :: Integer -> Poly
embed n = fit $ Poly [n]

-- | Multiplication in extended finite field.  Note that 'divModE' and 'euclides' can only
-- be applied to "equal-length" polynomials.
{-# INLINABLE (\*\) #-}
(\*\) :: Poly -> Poly -> Poly
(\*\) (Poly xs) (Poly ys) = Poly $ mod0 <$> zs
  where
    tailZeroes = replicate k0 0
    x = Poly $ xs <> tailZeroes
    y = Poly $ ys <> tailZeroes
    o = Poly $ poly0 <> tailZeroes
    Poly zs' = mod (x * y) o
    zs = take (k0 + 1) zs'

-- | Inverse operator in extended finite field
{-# INLINABLE inverse_P #-}
inverse_P :: Poly -> Poly
inverse_P x = mod t p0
  where
    p0 = Poly poly0
    (_, _, t) = euclides_P p0 x


-- Elliptic Curve over base field (Integers) --

data EllipticCurve_I = ECI Integer Integer | InftyI
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

unstableMakeIsData ''EllipticCurve_I

instance Eq EllipticCurve_I where
  (==) (ECI x1 y1) (ECI x2 y2) = mod0 (x2 - x1) == zero && mod0 (y2 - y1) == zero
  (==) (ECI _ _) InftyI        = False
  (==) InftyI (ECI _ _)        = False
  (==) InftyI InftyI           = True

-- | Group multiplication on an elliptic curve over the integers
instance Semigroup EllipticCurve_I where
  {-# INLINABLE (<>) #-}
  (<>) InftyI p = p
  (<>) p InftyI = p
  (<>) (ECI x1 y1) (ECI x2 y2)
    | x1 === x2 && y1 === (negate y2) = InftyI
    | x1 === x2 && y1 === y2          = let
           (x, y) = (x1, y1)
           m  = 3 \* (x \* x) \* (inverse_I $ 2 \* y)
           x' = m \* m - 2 \* x
           y' = m \* (x - x') - y
       in
           ECI (mod0 x') (mod0 y')
    | not (x1 === x2)                 = let
           m  = (y2 - y1) \* inverse_I (x2 - x1)
           x3 = m \* m - x1 - x2
           y3 = m \* (x1 - x3) - y1
       in
           ECI (mod0 x3) (mod0 y3)


-- Elliptic Curve over extension field (Polynomials) --

data EllipticCurve_P = ECP Poly Poly | InftyP
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

unstableMakeIsData ''Poly
unstableMakeIsData ''EllipticCurve_P

instance Eq EllipticCurve_P where
  (==) (ECP x1 y1) (ECP x2 y2) = mod0 (x2 - x1) == zero && mod0 (y2 - y1) == zero
  (==) (ECP _ _) InftyP        = False
  (==) InftyP (ECP _ _)        = False
  (==) InftyP InftyP           = True

-- | Group multiplication on an elliptic curve over polynomials
instance Semigroup EllipticCurve_P where
  (<>) InftyP p = p
  (<>) p InftyP = p
  (<>) (ECP x1 y1) (ECP x2 y2)
    | x1 === x2 && y1 === (negate y2) = InftyP
    | x1 === x2 && y1 === y2          = let
           (x, y) = (x1, y1)
           m  = embed 3 \*\ (x \*\ x) \*\ (inverse_P $ embed 2 \*\ y)
           x' = m \*\ m - embed 2 \*\ x
           y' = m \*\ (x - x') - y
       in
           ECP (mod0 x') (mod0 y')
    | not (x1 === x2)                 = let
           m  = (y2 - y1) \*\ inverse_P (x2 - x1)
           x3 = m \*\ m - x1 - x2
           y3 = m \*\ (x1 - x3) - y1
       in
           ECP (mod0 x3) (mod0 y3)


-- Pairing --

-- | Miller's algorithm
miller :: [Integer] -> EllipticCurve_P -> EllipticCurve_P -> Poly
miller !bits !p !q
  | p == InftyP || q == InftyP || p == q = embed r_sign
  | otherwise = millerGeneric bits p q

-- | Miller's algorithm: generic case
millerGeneric :: [Integer] -> EllipticCurve_P -> EllipticCurve_P -> Poly
millerGeneric bits (ECP xP yP) (ECP xQ yQ) = g1 \*\ (inverse_P g2)
  where
    g1                 = g1' \*\ (xQ - xT)
    (g1', g2, xT, _yT) = foldr comb (one, one, xP, yP) bits'
    comb               = millerComb (ECP xP yP) (ECP xQ yQ)
    bits'              = take (length bits - 1) bits

-- | Accumulator function for Miller's algorithm
millerComb :: EllipticCurve_P -> EllipticCurve_P                                  -- point parameters
              -> Integer -> (Poly, Poly, Poly, Poly) -> (Poly, Poly, Poly, Poly)  -- accumulator function
millerComb (ECP xP yP) (ECP xQ yQ) b (f1, f2, x, y) =
  let m   = ((embed 3) \*\ x \*\ x) \*\ (inverse_P ((embed 2) \*\ y))
      f1' = f1 \*\ f1 \*\ (yQ - y - m \*\ (xQ - x))
      f2' = f2 \*\ f2 \*\ (xQ + (embed 2) \*\ x - m \*\ m)
      x'  = m \*\ m - (embed 2) \*\ x
      y'  = negate y - m \*\ (x' - x)
  in  if b == 0 || x' - xP === zero
         then (f1', f2', x', y')
         else if b == 1
                 then let m'   = (y' - yP) \*\ (inverse_P (x' - xP))
                          f1'' = f1' \*\ (yQ - y' - m' \*\ (xQ - x'))
                          f2'' = f2' \*\ (xQ + (xP + x') - m' \*\ m')
                          x''  = m' \*\ m' - x' - xP
                          y''  = negate y' - m' \*\ (x'' - x')
                      in  (f1'', f2'', x'', y'')
                 else traceError "not binary"

-- | Embed into elliptic curve over field extension
embedEC :: EllipticCurve_I -> EllipticCurve_P
embedEC InftyI    = InftyP
embedEC (ECI n m) = ECP (embed n) (embed m)

type G1 = EllipticCurve_I  -- elliptic curve over the integers
type G2 = EllipticCurve_P  -- elliptic curve over polynomials

-- | Pairing function
pairing :: G1 -> G2 -> Poly
pairing !p1 !q2 = (embed r_sign) \*\ f_P_Q \*\ (inverse_P f_Q_P)
  where
    f_P_Q   = miller' (embedEC p1) q2
    f_Q_P   = miller' q2 (embedEC p1)
    miller' = miller bitsR  -- 'bitsR' defined in module 'Params'


-- 'Instance' power of g1 --

-- | Binary representation of a non-negative integer. Note that 'bits n' never
-- has a '0' head.
bits :: Integer -> [Integer]
bits n = go n []
  where
    go !m bits
      | m == 0    = bits
      | otherwise = let (q, r) = divMod m 2 in
          go q (r : bits)

-- | Effiicient exponentiation on 'G1'
exp1 :: G1 -> Integer -> G1
exp1 InftyI _ = InftyI
exp1 !p1 !n   = foldr (exp1Comb p1) InftyI (reverse $ bits n)

-- | Accumulator function for 'exp1'
exp1Comb :: G1 -> Integer -> G1 -> G1
exp1Comb !p1 !b !p = let p' = p <> p in
  if b == 0 then p' else p1 <> p'

type PublicInst = [Integer]

-- | 'Instance' power of g1
powInstance :: [G1] -> PublicInst -> G1
powInstance p1s ns = if length p1s == length ns + 1
  then foldr (<>) InftyI $ zipWith exp1 p1s ( [1] <> ns )
  else traceError "length mismatch"



-----------------------------------------------------------------------
----------------------------   Validator   ----------------------------
-----------------------------------------------------------------------

data ZKProof = ZKProof
  { g1A :: G1
  , g2B :: G2
  , g1C :: G1
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

unstableMakeIsData ''ZKProof

data VerifyDatum = VerifyDatum
  { g1Alpha :: G1
  , g2Beta  :: G2
  , g2Gamma :: G2
  , g2Delta :: G2
  , g1IC    :: [G1]
  , public  :: PublicInst  -- public instance
  , proof   :: ZKProof     -- zk proof
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

unstableMakeIsData ''VerifyDatum

-- | Same as "mkVerificationValidator" below, but ommiting the
-- 'ScriptContext' argument.  Used for testing in the REPL.
mkVerificationValidator' :: VerifyDatum -> () -> Bool
mkVerificationValidator' dat _ = traceIfFalse "REJECT" acceptQ
  where
    acceptQ :: Bool
    acceptQ = pairing g1A' g2B' == (pairing g1Alpha' g2Beta') \*\ (pairing g1I g2Gamma') \*\ (pairing g1C' g2Delta')

    g1I :: G1
    g1I = powInstance (g1IC dat) (public dat)

    g1A' = g1A $ proof dat
    g2B' = g2B $ proof dat
    g1C' = g1C $ proof dat

    g1Alpha' = g1Alpha $ dat
    g2Beta'  = g2Beta  $ dat
    g2Gamma' = g2Gamma $ dat
    g2Delta' = g2Delta $ dat



--------------------
----- PlutusTx -----

mkVerificationValidator :: VerifyDatum -> () -> PlutusV2.ScriptContext -> Bool
mkVerificationValidator dat _ _ = traceIfFalse "REJECT" acceptQ
  where
    acceptQ :: Bool
    acceptQ = pairing g1A' g2B' == (pairing g1Alpha' g2Beta') \*\ (pairing g1I g2Gamma') \*\ (pairing g1C' g2Delta')

    g1I :: G1
    g1I = powInstance (g1IC dat) (public dat)

    g1A' = g1A $ proof dat
    g2B' = g2B $ proof dat
    g1C' = g1C $ proof dat

    g1Alpha' = g1Alpha $ dat
    g2Beta'  = g2Beta  $ dat
    g2Gamma' = g2Gamma $ dat
    g2Delta' = g2Delta $ dat



data TypedGroth
instance PlutusV2.ValidatorTypes TypedGroth where
  type instance DatumType TypedGroth    = VerifyDatum
  type instance RedeemerType TypedGroth = ()

typedGrothValidator :: PlutusV2.TypedValidator TypedGroth
typedGrothValidator = V2.mkTypedValidator @TypedGroth
  $$(PlutusTx.compile [|| mkVerificationValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = PlutusV2.mkUntypedValidator

grothValidator :: PlutusV2.Validator
grothValidator = PlutusV2.validatorScript typedGrothValidator
