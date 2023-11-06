
module Algebra.V3.Types where

import           PlutusTx.Prelude (Eq, Ring)
import           Prelude          hiding (Eq)

-- | The 'Field' class
class (Eq a, Ring a) => Field a where
  mod0    :: a -> a
  (===)   :: a -> a -> Bool  -- Congruence

-- | The 'EuclideanRing' class
class Ring a => EuclideanRing a where
  divModE  :: a -> a -> (a, a)
  mod      :: a -> a -> a
