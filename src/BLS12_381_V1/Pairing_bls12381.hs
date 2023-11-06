{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use -" #-}
{-# HLINT ignore "Use negate" #-}
{-# HLINT ignore "Redundant fromInteger" #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BangPatterns        #-}

{-|
Module      : Pairing_bls12381
Description : Pairing over the BLS12-381 elliptic curve
Copyright   : (c) Eric Schorn, NCC Group Plc, 2020
License     : BSD-3
Maintainer  : eric.schorn@nccgroup.com
Stability   : experimental

Implements the BLS12-381 point generation and pairing calculation per
<https://electriccoin.co/blog/new-snark-curve/>.
This code has no dependencies and utilizes no language options.

This code is for experimental purposes: simplicity and clarity are the primary goals,
so the code may be incomplete, inefficient, incorrect and/or insecure. Specifically,
both the algorithms within the code and (the use of) Haskell's arbitrary-precision
integers are clearly not constant-time and thus introduce timing side channels. This
code has not undergone a security audit; use at your own risk.

Note that field element constructors are not exported. Valid points can either be
constructed directly via the `g1Point` or `g2Point` functions, or they can be
constructed from the `pointMul` function given a scalar and typically either the
`g1Generator` or `g2Generator` points. Points are then used in the `pairing` function
which returns `Fq12` elements that can be tested for equality, shown or used in more
elaborate calculations (involving multiplication) with the exported Fq12 `*` operator
(see the example below).

Coordinates are forced into Zp and Points are internally validated to be on-curve. As
such, the `g1Point`, `g1Generator`, `g2Point`, `g2Generator` and `pointMul` functions
all return `Maybe Points` which will need to be unwrapped prior to use in the
`pairing` function.

__Example usage:__

Demonstrate the following equality (note the constants shifting positions):

@pairing((12+34)*56*g1, 78*g2) == pairing(78*g1, 12*56*g2) * pairing(78*g1, 34*56*g2)@

where g1 and g2 are the standard group generators. Below is an example @ghci@
interpreter session.

@\$ ghci Crypto\/Pairing_bls12381.hs
\
*Pairing_bls12381> p_12p34m56 = g1Generator >>= pointMul ((12 + 34) * 56)
*Pairing_bls12381> q_78 = g2Generator >>= pointMul 78
*Pairing_bls12381> leftSide = pairing \<$\> p_12p34m56 \<*\> q_78 >>= id
*Pairing_bls12381>
*Pairing_bls12381> p_78 = g1Generator >>= pointMul 78
*Pairing_bls12381> q_12m56 = g2Generator >>= pointMul (12 * 56)
*Pairing_bls12381> q_34m56 = g2Generator >>= pointMul (34 * 56)
*Pairing_bls12381> pair2 = pairing \<$\> p_78 \<*\> q_12m56 >>= id
*Pairing_bls12381> pair3 = pairing \<$\> p_78 \<*\> q_34m56 >>= id
*Pairing_bls12381> rightSide = (*) \<$\> pair2 \<*\> pair3
*Pairing_bls12381>
*Pairing_bls12381> (==) \<$\> leftSide \<*\> rightSide
Just True@
-}

module BLS12_381_V1.Pairing_bls12381 where



import           Data.List          (unfoldr)
import           Data.Maybe         (fromJust)
import           Numeric            (showHex)
import           PlutusCore         (DefaultUni)
import           PlutusCore.Default (DefaultFun)
import           PlutusTx           as Tx (compile, getPlc)
import           PlutusTx.Builtins  (Integer)
import           PlutusTx.Prelude   hiding (fromInteger, inv)
import qualified PlutusTx.Prelude   as P
import           Prelude            (Show, String, show)
import qualified UntypedPlutusCore  as UPLC


-- Tower extension fields in t, u, v and w. See https://eprint.iacr.org/2009/556.pdf
newtype Fq1 = Fq1 {t0 :: Integer}
  deriving (Show)

data Fq2 = Fq2 {u1 :: Fq1, u0 :: Fq1}
  deriving (Show)


data Fq6 = Fq6 {v2 :: Fq2, v1 :: Fq2, v0 :: Fq2}
  deriving (Show)

data Fq12 = Fq12 {w1 :: Fq6, w0 :: Fq6}
  deriving (Show)


-- | The field prime constant used in BLS12-381 is exported for reference.
fieldPrime :: Integer
fieldPrime = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab



odd :: Integer -> Bool
odd i = not (even i)



class Field a where
  inv:: a -> a
  mul_nonres :: a -> a  -- Multiply by non-reducible polynomial

class FromInteger a where
  fromInteger :: Integer -> a

-- Fq1 is 'standard' single-element finite field
instance AdditiveSemigroup Fq1 where
  (+) (Fq1 a0) (Fq1 b0) = Fq1 ((a0 + b0) `modulo` fieldPrime)

instance AdditiveMonoid Fq1 where
  zero = Fq1 0

instance AdditiveGroup Fq1 where
  (-) (Fq1 a0) (Fq1 b0) = Fq1 ((a0 - b0) `modulo` fieldPrime)

instance MultiplicativeSemigroup Fq1 where
  (*) (Fq1 a0) (Fq1 b0) = Fq1 ((a0 * b0) `modulo` fieldPrime)

instance FromInteger Fq1 where
  fromInteger a0 = Fq1 (a0 `modulo` fieldPrime)

instance Eq Fq1 where
  (==) (Fq1 a0) (Fq1 b0) = a0 == b0


instance Field Fq1 where
  mul_nonres a0 = a0

  -- All fields inverse (incl 0) arrive here
  inv (Fq1 a0) = if a0 == 0 then traceError "inv of 0"
                            else Fq1 (beea a0 fieldPrime 1 0 fieldPrime)


-- Binary Extended Euclidean Algorithm (note that there are no divisions)
-- See: Guide to Elliptic Curve Cryptography by Hankerson, Menezes, and Vanstone
beea :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
beea u v x1 x2 p
  | not (u > 0 && v > 0) = trace "trace" u
  | u == 1 = modulo x1 p
  | v == 1 = modulo x2 p
  | even u = if even x1 then beea (shiftR u 1) v (shiftR x1 1) x2 p
                        else beea (shiftR u 1) v (shiftR (x1 + p) 1) x2 p
  | even v = if even x2 then beea u (shiftR v 1) x1 (shiftR x2 1) p
                        else beea u (shiftR v 1) x1 (shiftR (x2 + p) 1) p
  | u >= v = beea (u - v) v (x1 - x2) x2 p
  | u < v  = beea u (v - u) x1 (x2 - x1) p


-- Fq2 is constructed with Fq1(u) / (u^2 - β) where β = -1.
instance AdditiveSemigroup Fq2 where
  (+) (Fq2 a1 a0) (Fq2 b1 b0) = Fq2 (a1 + b1) (a0 + b0)

instance AdditiveMonoid Fq2 where
  zero = fromInteger 0

instance AdditiveGroup Fq2 where
  (-) (Fq2 a1 a0) (Fq2 b1 b0) = Fq2 (a1 - b1) (a0 - b0)

instance MultiplicativeSemigroup Fq2 where
  -- Opportunity for Karatsuba optimization: https://en.wikipedia.org/wiki/Karatsuba_algorithm
  (*) (Fq2 a1 a0) (Fq2 b1 b0) = Fq2 (a1 * b0 + a0 * b1) (a0 * b0 - a1 * b1)

instance FromInteger Fq2 where
  fromInteger a0 = Fq2 (fromInteger 0) (fromInteger a0)

instance Eq Fq2 where
  (==) (Fq2 a1 a0) (Fq2 b1 b0) = a1 == b1 && a0 == b0


instance Field Fq2 where
  mul_nonres (Fq2 a1 a0) = Fq2 (a1 + a0) (a0 - a1)
  inv (Fq2 a1 a0) = Fq2 ((negate a1) * factor) (a0 * factor)
    where
      !factor = inv (a1 * a1 + a0 * a0)


-- Fq6 is constructed with Fq2(v) / (v^3 - ξ) where ξ = u + 1
instance AdditiveSemigroup Fq6 where

  (+) (Fq6 a2 a1 a0) (Fq6 b2 b1 b0) = Fq6 (a2 + b2) (a1 + b1) (a0 + b0)

instance AdditiveMonoid Fq6 where
  zero = Fq6 (fromInteger 0) (fromInteger 0) (fromInteger 0)

instance AdditiveGroup Fq6 where
  (-) (Fq6 a2 a1 a0) (Fq6 b2 b1 b0) = Fq6 (a2 - b2) (a1 - b1) (a0 - b0)

instance MultiplicativeSemigroup Fq6 where
  -- Opportunity for Toom-Cook optimization: https://en.wikipedia.org/wiki/Toom%E2%80%93Cook_multiplication
  (*) (Fq6 a2 a1 a0) (Fq6 b2 b1 b0) = Fq6 t2 (t1 + t4) (t0 + t3)
    where
      !t0 = a0 * b0
      !t1 = a0 * b1 + a1 * b0
      !t2 = a0 * b2 + a1 * b1 + a2 * b0 -- check
      !t3 = mul_nonres (a1 * b2 + a2 * b1)
      !t4 = mul_nonres (a2 * b2)

instance FromInteger Fq6 where
  fromInteger a0 = Fq6 (fromInteger 0) (fromInteger 0) (fromInteger a0)

instance Eq Fq6 where
  (==) (Fq6 a2 a1 a0) (Fq6 b2 b1 b0) = a2 == b2 && a1 == b1 && a0 == b0


instance Field Fq6 where

  mul_nonres (Fq6 a2 a1 a0) = Fq6 a1 a0 (mul_nonres a2)
  inv (Fq6 a2 a1 a0) = Fq6 (t2 * factor) (t1 * factor) (t0 * factor)
    where
      !t0 = a0 * a0 - mul_nonres (a1 * a2)
      !t1 = mul_nonres (a2 * a2) - a0 * a1
      !t2 = a1 * a1 - a0 * a2
      !factor = inv (a0 * t0 + mul_nonres (a2 * t1) + mul_nonres (a1 * t2))


-- Fq12 is constructed with Fq6(w) / (w^2 - γ) where γ = v
instance AdditiveSemigroup Fq12 where

  (+) (Fq12 a1 a0) (Fq12 b1 b0) = Fq12 (a1 + b1) (a0 + b0)

instance AdditiveMonoid Fq12 where
  zero = Fq12 (fromInteger 0) (fromInteger 0)

instance AdditiveGroup Fq12 where
  (-) (Fq12 a1 a0) (Fq12 b1 b0) = Fq12 (a1 - b1) (a0 - b0)

instance MultiplicativeSemigroup Fq12 where
  -- Opportunity for Karatsuba optimization: https://en.wikipedia.org/wiki/Karatsuba_algorithm
  (*) (Fq12 !a1 !a0) (Fq12 !b1 !b0) = Fq12 (a1 * b0 + a0 * b1)
                                       (a0 * b0 + mul_nonres (a1 * b1))

instance FromInteger Fq12 where
  fromInteger a0 = Fq12 (fromInteger 0) (fromInteger a0)

instance Eq Fq12 where
  (==) (Fq12 a1 a0) (Fq12 b1 b0) = a1 == b1 && a0 == b0


instance Field Fq12 where
  mul_nonres _ = traceError "not needed for Fq12"

  inv (Fq12 a1 a0) = Fq12 ((negate a1) * factor) (a0 * factor)
    where
      !factor = inv (a0 * a0 - mul_nonres (a1 * a1))


-- Affine coordinates are used throughout for simplicity
data Point a = Affine {ax :: a, ay :: a}
             | PointAtInfinity
             deriving Show

instance (MultiplicativeSemigroup a) => MultiplicativeSemigroup (Point a) where
  (*) :: Point a -> Point a -> Point a
  (*) PointAtInfinity _             = PointAtInfinity
  (*) _ PointAtInfinity             = PointAtInfinity
  (*) (Affine x1 y1) (Affine x2 y2) = Affine (x1 * x2) (y1 * y2)

instance (Eq a) => Eq (Point a) where
  (==) PointAtInfinity PointAtInfinity = True
  (==) PointAtInfinity _               = False
  (==) _ PointAtInfinity               = False
  (==) (Affine x1 y1) (Affine x2 y2)   = x1 == x2 && y1 == y2


-- | The curve order constant of BLS12-381 is exported for reference.
groupOrder :: Integer
groupOrder = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001


-- | The standard generator point for G1.
g1Generator :: Maybe (Point Fq1)
g1Generator = Just (Affine (Fq1 0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb)
                           (Fq1 0x08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1))


-- | The standard generator point for G2.
g2Generator :: Maybe (Point Fq2)
g2Generator = Just (Affine (Fq2 (Fq1 0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e)
                                (Fq1 0x024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8))
                           (Fq2 (Fq1 0x0606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be)
                                (Fq1 0x0ce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801)))


-- BLS12-381 curve(s) are E1: y^2 = x^3 + 4 and E2: y^2 = x^3 + 4(u+1)
isOnCurve :: (Field a, Eq a, AdditiveSemigroup a, FromInteger a, MultiplicativeSemigroup a) => Point a -> Bool
isOnCurve PointAtInfinity = False
isOnCurve a0              = ay (a0 * a0) == (ax (a0 * a0 * a0) + mul_nonres (fromInteger 4))


-- Check subgroup=order membership (note pointMul calls isOnCurve)
isInSubGroup :: (Field a, Eq a, MultiplicativeSemigroup a, FromInteger a, AdditiveGroup a) => Point a -> Bool
isInSubGroup p = pointMul groupOrder p == Just PointAtInfinity


-- | Given @x@ and @y@, construct a valid point contained in G1.
g1Point :: Integer -> Integer -> Maybe (Point Fq1)
g1Point x y = if isInSubGroup candidate
              then Just candidate else Nothing
  where
    candidate = Affine (fromInteger x) (fromInteger y)


-- | Given @xi@, @x@, @yi@ and @y@, construct a valid point contained in G2.
g2Point :: Integer -> Integer -> Integer -> Integer -> Maybe (Point Fq2)
g2Point x1 x0 y1 y0 = if isInSubGroup candidate
                      then Just candidate else Nothing
  where
    !candidate = Affine (Fq2 (fromInteger x1) (fromInteger x0))
                       (Fq2 (fromInteger y1) (fromInteger y0))


-- While the use of Affine coordinates in pointAdd and pointMul stems from the
-- strong preference for simplicity, the inversion operation impacts performance.
-- This could be improved by using projective coordinates per https://eprint.iacr.org/2015/1060.pdf

-- | Add affine curve points; handle all corner cases
pointAdd :: (Field a, Eq a, MultiplicativeSemigroup a, AdditiveGroup a, FromInteger a) => Point a -> Point a -> Point a
pointAdd PointAtInfinity q = q
pointAdd p PointAtInfinity = p
pointAdd Affine {ax=x1, ay=y1} Affine {ax=x2, ay=y2}
  | x1 == x2 && y1 == y2 = pointDouble Affine {ax=x1, ay=y1}
  | x1 == x2 && y1 /= y2 = PointAtInfinity
  | otherwise = Affine {ax=x3, ay=y3}
  where
    slope = (y2 - y1) * inv (x2 - x1)
    x3 = (slope * slope) - x1 - x2
    y3 = slope * (x1 - x3) - y1


-- Double affine curve points
pointDouble :: (Field a, MultiplicativeSemigroup a, AdditiveGroup a, FromInteger a) => Point a -> Point a
pointDouble PointAtInfinity = PointAtInfinity
pointDouble Affine {ax=x1, ay=y1} = Affine {ax=x3, ay=y3}
  where
    !slope = (fromInteger 3 * (x1 * x1)) * inv (fromInteger 2 * y1)
    !x3 = (slope * slope) - x1 - x1
    !y3 = slope * (x1 - x3) - y1


-- Negate a curve Point
pointNegate :: ( MultiplicativeSemigroup a, FromInteger a) =>Point a -> Point a
pointNegate Affine {ax=x1, ay=y1} = Affine {ax=x1, ay= fromInteger (-1)*y1}


-- | Multiply an integer scalar and valid point in either G1 or G2.
pointMul :: (Field a, Eq a, MultiplicativeSemigroup a, FromInteger a, AdditiveGroup a) => Integer -> Point a -> Maybe (Point a)
pointMul !scalar !base
  | isOnCurve base && scalar > 0 = Just (pointMul' scalar base PointAtInfinity)
  | isOnCurve base && scalar < 0 = Just (pointMul' ((negate 1)*scalar) (pointNegate base)
                                         PointAtInfinity)
  | otherwise = Nothing


-- Double and add helper loop
pointMul' :: (Field a, Eq a, MultiplicativeSemigroup a, AdditiveGroup a, FromInteger a) => Integer -> Point a -> Point a -> Point a
pointMul' !scalar !base !accum
  | scalar == 0 = accum
  | odd scalar  = pointMul' (shiftR scalar 1) doubleBase (pointAdd accum base)
  | even scalar = pointMul' (shiftR scalar 1) doubleBase accum
  where
    !doubleBase = pointAdd base base


-- Untwist point on E2 for pairing calculation
untwist :: Point Fq2 -> Point Fq12
untwist Affine {ax=x1, ay=y1} = Affine {ax=wideX, ay=wideY}
  where
    !root = Fq6 (fromInteger 0) (fromInteger 1) (fromInteger 0)
    !wideX = Fq12 (fromInteger 0) (Fq6 (fromInteger 0) (fromInteger 0) x1) * inv (Fq12 (fromInteger 0) root)
    !wideY = Fq12 (fromInteger 0) (Fq6 (fromInteger 0) (fromInteger 0) y1) * inv (Fq12 root (fromInteger 0))

-- Used in miller loop for computing line functions l_r,r and v_2r
doubleEval :: Point Fq2 -> Point Fq1 -> Fq12
doubleEval !r !p = fromInteger (t0 (ay p)) - (fromInteger (t0 (ax p)) * slope) - v
  where
    !wideR = untwist r
    !slope = (fromInteger 3 * ax wideR * ax wideR) * inv (fromInteger 2 * ay wideR)
    !v = ay wideR - slope * ax wideR


-- Used in miller loop for computer line function l_r,p and v_r+p
addEval :: Point Fq2 -> Point Fq2 -> Point Fq1 -> Fq12
addEval !r !q !p = if (ax wideR == ax wideQ) && (ay wideR == negate (ay wideQ))
                then fromInteger (t0 (ax p)) - ax wideR
                else addEval' wideR wideQ p
  where
    !wideR = untwist r
    !wideQ = untwist q


-- Helper function for addEval
addEval' :: Point Fq12 -> Point Fq12 -> Point Fq1 -> Fq12
addEval' !wideR !wideQ !p = fromInteger (t0 (ay p)) - (fromInteger (t0 (ax p)) * slope) - v
  where
    !slope = (ay wideQ - ay wideR) * inv (ax wideQ - ax wideR)
    !v = ((ay wideQ * ax wideR) - (ay wideR * ax wideQ)) * inv (ax wideR - ax wideQ)


-- Classic Miller loop for Ate pairing
miller :: Point Fq1 -> Point Fq2 -> Fq12
miller !p !q = miller' p q (pointNegate q) iterations (fromInteger 1)

iterations :: [Bool]
iterations = tail $ reverse $ -- list of true/false per bits of operand
  unfoldr (\b -> if b == (0 :: Integer) then Nothing
                  else Just(odd b, shiftR b 1)) 0xd201000000010000


-- Double and add loop helper for Miller (iterative)
miller' :: Point Fq1 -> Point Fq2 -> Point Fq2 -> [Bool] -> Fq12 -> Fq12
miller' !p !q !t [] !f = f
miller' !p !q !t (i:iters) !f =
  if i then miller' p q (pointAdd doubleT (pointNegate q)) iters (accum * addEval doubleT (pointNegate q) p)
       else miller' p q doubleT iters accum
  where
    !accum = f * f * doubleEval t p
    !doubleT = pointDouble t


(^) :: ( MultiplicativeMonoid a) =>a -> Integer -> a
(^) !b !e              = if e == 0 then one else b * (b ^ (e - 1))

-- | Pairing calculation for a valid point in G1 and another valid point in G2.
pairing :: Point Fq1 -> Point Fq2 -> Maybe Fq12
pairing p_g1 q_g2
  | p_g1 == PointAtInfinity || q_g2 == PointAtInfinity = Nothing
  | isOnCurve p_g1 && isInSubGroup p_g1 && isOnCurve q_g2 && isInSubGroup q_g2
      -- = Just (pow' (miller p_g1 q_g2) (divide (fieldPrime^12 - 1) groupOrder) (fromInteger 1))
      = Just (miller p_g1 q_g2)
  | otherwise = Nothing


-- Used for the final exponentiation; opportunity for further perf optimization
pow' :: (Field a, MultiplicativeSemigroup a) => a -> Integer -> a -> a
pow' !a0 !exp !result
  | exp <= 1 = a0
  | even exp = accum * accum
  | otherwise = accum * accum * a0
  where accum = pow' a0 (shiftR exp 1) result



-- | A test vector for the pairing calculation.

testVector :: Fq12
testVector = fromMaybe (fromInteger 0) $ pairing
  pointP
  pointQ


testPlutusBLSV1 :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
testPlutusBLSV1 = getPlc $$(Tx.compile [|| testVector ||])


shiftR :: Integer -> Integer -> Integer
shiftR !a !b = a `divide` (2 ^ b)


pointP = Affine (Fq1 0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb)
    (Fq1 0x08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1)

pointQ = Affine (Fq2 (Fq1 0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e)
    (Fq1 0x024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8))
    (Fq2 (Fq1 0x0606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be)
    (Fq1 0x0ce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801))

e0 :: Fq12 -> String
e0 Fq12 { w0=Fq6{v0=Fq2{u0=Fq1{t0=e}}}} = showHex e ""

e1 :: Fq12 -> String
e1 Fq12 { w0=Fq6{v0=Fq2{u1=Fq1{t0=e}}}} = showHex e ""

e2 :: Fq12 -> String
e2 Fq12 { w0=Fq6{v1=Fq2{u0=Fq1{t0=e}}}} = showHex e ""

e3 :: Fq12 -> String
e3 Fq12 { w0=Fq6{v1=Fq2{u1=Fq1{t0=e}}}} = showHex e ""

e4 :: Fq12 -> String
e4 Fq12 { w0=Fq6{v2=Fq2{u0=Fq1{t0=e}}}} = showHex e ""

e5 :: Fq12 -> String
e5 Fq12 { w0=Fq6{v2=Fq2{u1=Fq1{t0=e}}}} = showHex e ""

e6 :: Fq12 -> String
e6 Fq12 { w1=Fq6{v0=Fq2{u0=Fq1{t0=e}}}} = showHex e ""

e7 :: Fq12 -> String
e7 Fq12 { w1=Fq6{v0=Fq2{u1=Fq1{t0=e}}}} = showHex e ""

e8 :: Fq12 -> String
e8 Fq12 { w1=Fq6{v1=Fq2{u0=Fq1{t0=e}}}} = showHex e ""

e9 :: Fq12 -> String
e9 Fq12 { w1=Fq6{v1=Fq2{u1=Fq1{t0=e}}}} = showHex e ""

e10 :: Fq12 -> String
e10 Fq12 { w1=Fq6{v2=Fq2{u0=Fq1{t0=e}}}} = showHex e ""

e11 :: Fq12 -> String
e11 Fq12 { w1=Fq6{v2=Fq2{u1=Fq1{t0=e}}}} = showHex e ""


showe0 :: String
showe0 = e0  testVector

showe1 :: String
showe1 = e1  testVector

showe2 :: String
showe2 = e2  testVector

showe3 :: String
showe3 = e3  testVector

showe4 :: String
showe4 = e4  testVector

showe5 :: String
showe5 = e5  testVector

showe6 :: String
showe6 = e6  testVector

showe7 :: String
showe7 = e7  testVector

showe8 :: String
showe8 = e8  testVector

showe9 :: String
showe9 = e9  testVector

showe10 :: String
showe10 = e10  testVector

showe11 :: String
showe11 = e11  testVector


fq12_1 = fromInteger 1 :: Fq12
fq12_2 = fromInteger 2 :: Fq12


fq12_1x2 = fq12_1 * fq12_2

lttp = doubleEval (fromJust g2Generator) (fromJust g1Generator)
