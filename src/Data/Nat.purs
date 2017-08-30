module Data.Nat
  ( class IsNat
  , reflectNat
  , reifyNat
  , NProxy(..)
  ) where

import Prelude ((<), negate)
import Unsafe.Coerce (unsafeCoerce)

-- | A value-level proxy for a type-level natural.
data NProxy (nat :: Nat) = NProxy

-- | A class for known naturals
class IsNat (nat :: Nat) where
  reflectNat :: NProxy nat -> Int

reifyNat :: forall r. Int -> (forall nat. IsNat nat => NProxy nat -> r) -> r
reifyNat n f = coerce f { reflectNat: \_ -> abs n } NProxy where
  coerce
    :: (forall n. IsNat n                           => NProxy n -> r)
    -> (forall n. { reflectNat :: NProxy n -> Int } -> NProxy n -> r)
  coerce = unsafeCoerce
  abs x = if x < 0 then -x else x
