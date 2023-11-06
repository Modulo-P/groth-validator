{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}



module BLS12_381_V2.Pairing_bls12381 where

import           Cardano.Prelude    (FatalError (fatalErrorMessage),
                                     Integral (quot), conjugate, testBitDefault,
                                     unfoldr)
import           Data.List          (unfoldr)
import           Data.Maybe         (fromJust)
import           Debug.Trace        (trace)
import           Numeric            (showHex)
import           PlutusCore         (DefaultUni, freshName)
import           PlutusCore.Default (DefaultFun)
import           PlutusTx           as Tx (compile, getPlc)
import           PlutusTx.Builtins  (Integer)
import           PlutusTx.Prelude   hiding (fromInteger, inv, trace)
import qualified PlutusTx.Prelude   as P
import           Prelude            (Show, String, show, undefined)
import qualified UntypedPlutusCore  as UPLC

fieldPrime :: Integer
fieldPrime = 0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab

-- | The curve order constant of BLS12-381 is exported for reference.
groupOrder :: Integer
groupOrder = 0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001

odd :: Integer -> Bool
odd i = not (even i)

class FromInteger a where
  fromInteger :: Integer -> a

class Field a where
  inv:: a -> a
  mul_nonres :: a -> a
  frobenius :: a -> a

-- Tower extension fields in t, u, v and w. See https://eprint.iacr.org/2009/556.pdf

-- |********************************|
-- |***         Fq1              ***|
-- |********************************|
newtype Fq1 = Fq1 {t0 :: Integer} deriving (Show)

instance Eq Fq1 where
  (==) (Fq1 a) (Fq1 b) = a == b

instance AdditiveSemigroup Fq1 where
  (+) (Fq1 a) (Fq1 b) = Fq1 $ (a + b) `modulo` fieldPrime

instance AdditiveMonoid Fq1 where
  zero = Fq1 0

instance AdditiveGroup Fq1 where
  (-) (Fq1 a) (Fq1 b)= Fq1 $ (a - b) `modulo` fieldPrime

instance MultiplicativeSemigroup Fq1 where
  (*) (Fq1 !a0) (Fq1 !b0) = Fq1 ((a0 * b0) `modulo` fieldPrime)

instance MultiplicativeMonoid Fq1 where
  one = Fq1 1

instance FromInteger Fq1 where
  fromInteger a0 = Fq1 (a0 `modulo` fieldPrime)

instance Field Fq1 where
  mul_nonres a0 = a0

  -- All fields inverse (incl 0) arrive here
  inv (Fq1 !a0) = if a0 == 0 then error ()
                            else Fq1 (beea a0 fieldPrime 1 0 fieldPrime)

  frobenius (Fq1 !a0) = Fq1 a0


-- Binary Extended Euclidean Algorithm (note that there are no divisions)
-- See: Guide to Elliptic Curve Cryptography by Hankerson, Menezes, and Vanstone
beea :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
beea u v x1 x2 p
  | not (u > 0 && v > 0) = error ()
  | u == 1 = modulo x1 p
  | v == 1 = modulo x2 p
  | even u = if even x1 then beea (shiftR u 1) v (shiftR x1 1) x2 p
                        else beea (shiftR u 1) v (shiftR (x1 + p) 1) x2 p
  | even v = if even x2 then beea u (shiftR v 1) x1 (shiftR x2 1) p
                        else beea u (shiftR v 1) x1 (shiftR (x2 + p) 1) p
  | u >= v = beea (u - v) v (x1 - x2) x2 p
  | u < v  = beea u (v - u) x1 (x2 - x1) p
  | otherwise = error ()

shiftR :: Integer -> Integer -> Integer
shiftR !a !b = a `divide` (2 ^ b)

infixr 8 ^
(^) :: ( MultiplicativeMonoid a) =>a -> Integer -> a
(^) b e              = if e == 0 then one else b * b ^ (e - 1)

-- |********************************|
-- |***         Fq2              ***|
-- |********************************|
data Fq2 = Fq2 {u1 :: Fq1, u0 :: Fq1}
  deriving (Show)

instance Eq Fq2 where
  (==) (Fq2 a1 a0) (Fq2 b1 b0) = a1 == b1 && a0 == b0

instance AdditiveSemigroup Fq2 where
  (+) (Fq2 a1 a0) (Fq2 b1 b0) = Fq2 (a1 + b1) (a0 + b0)

instance AdditiveMonoid Fq2 where
  zero = fromInteger 0

instance AdditiveGroup Fq2 where
  (-) (Fq2 a1 a0) (Fq2 b1 b0) = Fq2 (a1 - b1) (a0 - b0)

instance MultiplicativeSemigroup Fq2 where
  -- Opportunity for Karatsuba optimization: https://en.wikipedia.org/wiki/Karatsuba_algorithm
  (*) (Fq2 !a1 !a0) (Fq2 !b1 !b0) = Fq2 (a1 * b0 + a0 * b1) (a0 * b0 - a1 * b1)

instance MultiplicativeMonoid Fq2 where
  one = fromInteger 1

instance FromInteger Fq2 where
  fromInteger a0 = Fq2 (fromInteger 0) (fromInteger a0)

instance Field Fq2 where
  mul_nonres (Fq2 a1 a0) = Fq2 (a1 + a0) (a0 - a1)
  inv (Fq2 !a1 !a0) = Fq2 (negate a1 * factor) (a0 * factor)
    where
      !factor = inv (a1 * a1 + a0 * a0)
  frobenius (Fq2 !a1 !a0) = Fq2 (negate a1) a0

-- |********************************|
-- |***         Fq6              ***|
-- |********************************|
data Fq6 = Fq6 {v2 :: Fq2, v1 :: Fq2, v0 :: Fq2}
  deriving (Show)

instance Eq Fq6 where
  (==) (Fq6 a2 a1 a0) (Fq6 b2 b1 b0) = a2 == b2 && a1 == b1 && a0 == b0

instance AdditiveSemigroup Fq6 where

  (+) (Fq6 a2 a1 a0) (Fq6 b2 b1 b0) = Fq6 (a2 + b2) (a1 + b1) (a0 + b0)

instance AdditiveMonoid Fq6 where
  zero = Fq6 (fromInteger 0) (fromInteger 0) (fromInteger 0)

instance AdditiveGroup Fq6 where
  (-) (Fq6 a2 a1 a0) (Fq6 b2 b1 b0) = Fq6 (a2 - b2) (a1 - b1) (a0 - b0)

instance MultiplicativeSemigroup Fq6 where
  -- Opportunity for Toom-Cook optimization: https://en.wikipedia.org/wiki/Toom%E2%80%93Cook_multiplication
  (*) (Fq6 !a2 !a1 !a0) (Fq6 !b2 !b1 !b0) = Fq6 c2 c1 c0
    where
      !t0 = a0 * b0
      !t1 = a1 * b1
      !t2 = a2 * b2
      !c0 = mul_nonres ((a1 + a2) * (b1 + b2) - t1 - t2) + t0
      !c1 = (a0 + a1) * (b0 + b1) - t0 - t1 + mul_nonres t2
      !c2 = (a0 + a2) * (b0 + b2) - t0 - t2 + t1


instance MultiplicativeMonoid Fq6 where
  one = Fq6 (fromInteger 0) (fromInteger 0) (fromInteger 1)

instance FromInteger Fq6 where
  fromInteger a0 = Fq6 (fromInteger 0) (fromInteger 0) (fromInteger a0)

instance Field Fq6 where

  mul_nonres (Fq6 a2 a1 a0) = Fq6 a1 a0 (mul_nonres a2)
  inv (Fq6 !a2 !a1 !a0) = Fq6 (t2 * factor) (t1 * factor) (t0 * factor)
    where
      !t0 = a0 * a0 - mul_nonres (a1 * a2)
      !t1 = mul_nonres (a2 * a2) - a0 * a1
      !t2 = a1 * a1 - a0 * a2
      !factor = inv (a0 * t0 + mul_nonres (a2 * t1) + mul_nonres (a1 * t2))
  frobenius (Fq6 !a2 !a1 !a0) = Fq6 (frobenius a2 * frobC2) (frobenius a1 * frobC1) (frobenius a0)
    where
        !frobC1 = Fq2 (Fq1 4002409555221667392624310435006688643935503118305586438271171395842971157480381377015405980053539358417135540939436) zero
        !frobC2 = Fq2 zero (Fq1 4002409555221667392624310435006688643935503118305586438271171395842971157480381377015405980053539358417135540939437)

-- |********************************|
-- |***         Fq12             ***|
-- |********************************|
data Fq12 = Fq12 {w1 :: Fq6, w0 :: Fq6}
  deriving (Show)

instance Eq Fq12 where
  (==) (Fq12 a1 a0) (Fq12 b1 b0) = a1 == b1 && a0 == b0

-- Fq12 is constructed with Fq6(w) / (w^2 - γ) where γ = v
instance AdditiveSemigroup Fq12 where

  (+) (Fq12 a1 a0) (Fq12 b1 b0) = Fq12 (a1 + b1) (a0 + b0)

instance AdditiveMonoid Fq12 where
  zero = Fq12 (fromInteger 0) (fromInteger 0)

instance AdditiveGroup Fq12 where
  (-) (Fq12 a1 a0) (Fq12 b1 b0) = Fq12 (a1 - b1) (a0 - b0)

conjugateFq12 :: Fq12 -> Fq12
conjugateFq12 (Fq12 a1 a0) = Fq12 (negate a1) a0

instance MultiplicativeSemigroup Fq12 where
  -- Opportunity for Karatsuba optimization: https://en.wikipedia.org/wiki/Karatsuba_algorithm
  (*) (Fq12 !a1 !a0) (Fq12 !b1 !b0) = Fq12 (a1 * b0 + a0 * b1)
                                       (a0 * b0 + mul_nonres (a1 * b1))

instance MultiplicativeMonoid Fq12 where
  one = Fq12 (fromInteger 0) (fromInteger 1)

instance FromInteger Fq12 where
  fromInteger a0 = Fq12 (fromInteger 0) (fromInteger a0)


instance Field Fq12 where
  mul_nonres _ = error ()

  inv (Fq12 a1 a0) = Fq12 (negate a1 * factor) (a0 * factor)
    where
      factor = inv (a0 * a0 - mul_nonres (a1 * a1))

  frobenius (Fq12 !a1 !a0) = Fq12 a1' (frobenius a0)
    where
        !frobC1 = Fq2 (Fq1 151655185184498381465642749684540099398075398968325446656007613510403227271200139370504932015952886146304766135027)
          (Fq1 3850754370037169011952147076051364057158807420970682438676050522613628423219637725072182697113062777891589506424760)
        Fq6 !c2 !c1 !c0 = frobenius a1
        !a1' = Fq6 (c2 * frobC1) (c1 * frobC1) (c0 * frobC1)


-- |********************************|
-- |***         Elliptic curve   ***|
-- |********************************|

data AffinePoint a = Affine {ax :: a, ay :: a}
             | PointAtInfinity
             deriving Show

instance (MultiplicativeSemigroup a) => MultiplicativeSemigroup (AffinePoint a) where
  (*) PointAtInfinity _             = PointAtInfinity
  (*) _ PointAtInfinity             = PointAtInfinity
  (*) (Affine x1 y1) (Affine x2 y2) = Affine (x1 * x2) (y1 * y2)

instance (Eq a) => Eq (AffinePoint a) where
  (==) PointAtInfinity PointAtInfinity = True
  (==) PointAtInfinity _               = False
  (==) _ PointAtInfinity               = False
  (==) (Affine x1 y1) (Affine x2 y2)   = x1 == x2 && y1 == y2

data JPoint a = JPoint {jx :: a, jy :: a, jz :: a}
             deriving Show


instance (MultiplicativeMonoid a, AdditiveGroup a, FromInteger a, Eq a) => AdditiveSemigroup (JPoint a) where

  -- add-2007-bl https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#addition-add-2007-bl
  (+) (JPoint x1 y1 z1) (JPoint x2 y2 z2)
    | z1 == zero || z2 == zero =
      if z1 == zero then
        JPoint x2 y2 z2
      else
        if z2 == zero then
          JPoint x1 y1 z1
        else
          jpointAtInfinity
    | u1 == u2 && s1 == s2 = doubleJacobianPoint (JPoint x1 y1 z1)
    | otherwise = if z3 == zero then jpointAtInfinity else JPoint x3 y3 z3
    where
        z1z1 = z1 ^ 2
        z2z2 = z2 ^ 2
        u1 = x1 * z2z2
        u2 = x2 * z1z1
        s1 = y1 * z2 * z2z2
        s2 = y2 * z1 * z1z1
        h = u2 - u1
        i = (fromInteger 2 * h) ^ 2
        j = h * i
        r = fromInteger 2 * (s2 - s1)
        v = u1 * i
        x3 = r ^ 2 - j - fromInteger 2 * v
        y3 = r * (v - x3) - fromInteger 2 * s1 * j
        z3 = ((z1 + z2) ^ 2 - z1z1 - z2z2) * h

jpointAtInfinity :: FromInteger a => JPoint a
jpointAtInfinity = JPoint (fromInteger 1) (fromInteger 1) (fromInteger 0)


jpointNegate :: AdditiveGroup a => JPoint a -> JPoint a
jpointNegate (JPoint x y z) = JPoint x (negate y) z

-- dbl-2009-l https://hyperelliptic.org/EFD/g1p/auto-shortw-jacobian-0.html#doubling-dbl-2009-l
doubleJacobianPoint :: (MultiplicativeMonoid a, AdditiveGroup a, FromInteger a, Eq a) => JPoint a -> JPoint a
doubleJacobianPoint (JPoint x1 y1 z1) = if z3 == zero then jpointAtInfinity else JPoint x3 y3 z3
  where
    a = x1 ^ 2
    b = y1 ^ 2
    c = b ^ 2
    d = fromInteger 2 * ((x1 + b) ^ 2 - a - c)
    e = fromInteger 3 * a
    f = e ^ 2
    x3 = f - fromInteger 2 * d
    y3 = e * (d - x3) - fromInteger 8 * c
    z3 = fromInteger 2 * y1 * z1

-- | Multiply an integer scalar and valid point in either G1 or G2.
jpointMul :: (Field a, Eq a, MultiplicativeMonoid a, AdditiveGroup a, FromInteger a) => Integer -> JPoint a -> Maybe (JPoint a)
jpointMul scalar base
  | isJacobianPointOnCurve base && scalar > 0 = Just (jpointMul' scalar base jpointAtInfinity)
  | isJacobianPointOnCurve base && scalar < 0 = Just (jpointMul' (-1*scalar) (jpointNegate base)
                                         jpointAtInfinity)
  | otherwise = Nothing


-- Double and add helper loop
jpointMul' :: (Field a, Eq a, MultiplicativeMonoid a, AdditiveGroup a, FromInteger a) => Integer -> JPoint a -> JPoint a -> JPoint a
jpointMul' scalar base accum
  | scalar == 0 = accum
  | odd scalar  = jpointMul' (shiftR scalar 1) doubleBase (accum + base)
  | even scalar = jpointMul' (shiftR scalar 1) doubleBase accum
  | otherwise   = error ()
  where
    doubleBase = doubleJacobianPoint base

jpointToAffine :: (MultiplicativeMonoid a, Field a) => JPoint a -> AffinePoint a
jpointToAffine (JPoint x1 y2 z3) = Affine (x1 * invZ33 * z3) (y2 * invZ33)
  where
    invZ33 =inv  z3 ^ 3


-- |********************************|
-- |***         Miller loop      ***|
-- |********************************|
lineDouble :: JPoint Fq2 -> AffinePoint Fq1 -> (Fq12, JPoint Fq2)
lineDouble q@JPoint{jx=xq, jy=yq, jz=zq} (Affine xp yp) = (Fq12 a1 a0, JPoint xt yt zt)
  where
      !zq2 = zq ^ 2
      !tmp0 = xq^2
      !tmp1 = yq^2
      !tmp2 = tmp1^2
      !tmp3' = (xq + tmp1)^2 - tmp0 - tmp2
      !tmp3'' = fromInteger 2 * tmp3'
      !tmp4 = fromInteger 3 * tmp0
      !tmp6 = xq + tmp4
      !tmp5 = tmp4^2
      !xt = tmp5 - fromInteger 2 * tmp3''
      !zt = (yq + zq)^2 - tmp1 - zq2
      !yt = (tmp3'' - xt) * tmp4 - fromInteger 8 * tmp2
      !tmp3''' = negate (fromInteger 2) * (tmp4 * zq2)
      !tmp3'''' = tmp3''' * Fq2 zero xp
      !tmp6' = tmp6 ^ 2 - tmp0 - tmp5 - fromInteger 4 * tmp1
      !tmp0' = fromInteger 2 * (zt * zq2)
      !tmp0'' = tmp0' * Fq2 zero yp
      !a0 = Fq6 zero tmp3'''' tmp6'
      !a1 = Fq6 zero tmp0'' zero
lineDouble _ _ = error ()

lineAdd :: JPoint Fq2 -> JPoint Fq2 -> AffinePoint Fq1 -> (Fq12, JPoint Fq2)
lineAdd r@JPoint{jx=xr, jy=yr, jz=zr} q@JPoint{jx=xq, jy=yq, jz=zq}  (Affine xp yp) = (Fq12 l1 l0, JPoint xt yt zt)
  where
    !zr2 = zr ^ 2
    !t0 = xq * zr2
    !t1 = (yq + zr) ^ 2 - yq ^ 2 - zr2
    !t1' = t1 * zr2
    !t2 = t0 - xr
    !t3 = t2 ^ 2
    !t4 = fromInteger 4 * t3
    !t5 =  t4 * t2
    !t6 = t1' - fromInteger 2 * yr
    !t9 = t6 * xq
    !t7 = xr * t4
    !xt = t6 ^2 - t5 - fromInteger 2 * t7
    !zt = (zr + t2) ^ 2 - zr2 - t3
    !t10 = yq + zt
    !t8 = (t7 - xt) * t6
    !t0' = fromInteger 2 * (yr * t5)
    !yt = t8 - t0'
    !t10' = t10 ^ 2 - yq ^ 2 - zt ^ 2
    !t9' = fromInteger 2 * t9 - t10'
    !t10'' = fromInteger 2 * (zt * Fq2 zero yp)
    !t6' = negate t6
    !t1'' =  fromInteger 2 * (t6' * Fq2 zero xp)
    !l0 = Fq6 zero t1'' t9'
    !l1 = Fq6 zero t10'' zero
lineAdd _ _ _ = error ()

traceMillerLoop :: (Show p, Show a) => p -> a -> a
traceMillerLoop step t = trace ("Step: " ++ show step ++ " " ++ show t) t

millerLoop :: AffinePoint Fq1 -> JPoint Fq2 -> Fq12 -> JPoint Fq2 -> [Integer] -> Integer -> (Fq12, JPoint Fq2)
millerLoop !p !q !f !t [] !step = (conjugateFq12 (f*ld), doublet)
  where
    (ld, doublet) = lineDouble t p
millerLoop !p !q !f !t (i:is) !step
  | i == 0 = millerLoop p q (f' ^ 2) doublet is (step + 1)
  | otherwise = millerLoop p q ((f' * la) ^ 2) addt is (step + 1)
  where
      (ld, doublet) = lineDouble t p
      f' = ld * f
      (la, addt) = lineAdd doublet q p

miller :: AffinePoint Fq1 -> JPoint Fq2 -> Fq12
miller p q@JPoint{jx=qx, jy=qy, jz=qz} = pow'' loop
  where
    f = one
    t = q
    (!loop, !tloop) = millerLoop p q f t iterations 1

pairing :: AffinePoint Fq1 -> JPoint Fq2 -> Fq12
pairing !p !q = m
  where
    !m = miller p q

t :: Integer
t = 0xd201000000010000

iterations :: [Integer]
iterations = tail $ reverse $ tail $
  unfoldr (\b -> if b == (0 :: Integer) then Nothing
                  else Just(if odd b then 1 else 0, shiftR b 1)) t

iterationsExp = reverse $
      unfoldr (\b -> if b == (0 :: Integer) then Nothing
                      else Just(if odd b then True else False, shiftR b 1)) t

pow' :: (Field a, MultiplicativeSemigroup a) => a -> [Bool] -> a -> a
pow' !a0 [] accum = accum
pow' !a0 (o:os) accum = pow' a0 os loop
  where
    !loop = if o then accum * accum * a0 else accum * accum

pow'' :: Fq12 -> Fq12
pow'' !f = f'
  where
    !t0 = frobenius $ frobenius $ frobenius $ frobenius $ frobenius $ frobenius f
    !t1 = inv f
    !t2 = t0 * t1
    !t1' = t2
    !t2' = frobenius $ frobenius t2
    !t2'' = t2' * t1'
    !t1'' = conjugateFq12 (t2'' * t2'')
    !t3 = inv(pow' t2'' iterationsExp one)
    !t4 = t3 * t3
    !t5 = t1'' * t3
    !t1''' = inv(pow' t5 iterationsExp one)
    !t0' = inv(pow' t1''' iterationsExp one)
    !t6 = inv(pow' t0' iterationsExp one)
    !t6' = t6 * t4
    !t4' = inv(pow' t6' iterationsExp one)
    !t5' = conjugateFq12 t5
    !t4''= t4' * t5' * t2''
    !t5'' = conjugateFq12 t2''
    !t1'''' = t1''' * t2''
    !t1''''' = frobenius $ frobenius $ frobenius t1''''
    !t6'' = t6' * t5''
    !t6''' = frobenius t6''
    !t3' = t3 * t0'
    !t3'' = frobenius $ frobenius t3'
    !t3''' = t3'' * t1'''''
    !t3'''' = t3''' * t6'''
    !f' = t3'''' * t4''





-- |********************************|
-- |***         Test             ***|
-- |********************************|

testVector :: Fq12
testVector = pairing g1Generator g2Generator

testFq12 :: Fq12
testFq12 = pow' (fromInteger 4) iterationsExp one

testPlutusBLSV2 :: UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
testPlutusBLSV2 = getPlc $$(Tx.compile [|| testVector ||])


fq6A :: Fq6
fq6A = Fq6 (Fq2 (Fq1 0) (Fq1 1)) (Fq2 (Fq1 0) (Fq1 1)) (Fq2 (Fq1 0) (Fq1 1))

fq6B :: Fq6
fq6B = fromInteger 2

test = fq6A * fq6B == fq6A + fq6A


-- |********************************|
-- |***         test Curve       ***|
-- |********************************|

isJacobianPointOnCurve :: (MultiplicativeMonoid a, AdditiveGroup a, Eq a, Field a, FromInteger a) => JPoint a -> Bool
isJacobianPointOnCurve (JPoint x y z) = y ^ 2 == x ^ 3 + mul_nonres (fromInteger 4) * z ^ 6


g2Generator :: JPoint Fq2
g2Generator = JPoint (Fq2 (Fq1 0x13e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e)
                                (Fq1 0x024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8))
                           (Fq2 (Fq1 0x0606c4a02ea734cc32acd2b02bc28b99cb3e287e85a763af267492ab572e99ab3f370d275cec1da1aaa9075ff05f79be)
                                (Fq1 0x0ce5d527727d6e118cc9cdc6da2e351aadfd9baa8cbdd3a76d429a695160d12c923ac9cc3baca289e193548608b82801))
                                (Fq2 (Fq1 0x0) (Fq1 0x1))

g1Generator :: AffinePoint Fq1
g1Generator = Affine (Fq1 0x17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb)
                           (Fq1 0x08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1)

checkG2 :: Bool
checkG2 = isJacobianPointOnCurve (doubleJacobianPoint g2Generator)

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



