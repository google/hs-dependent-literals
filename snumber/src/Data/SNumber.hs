-- Copyright 2020-2021 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

-- | Runtime witnesses of type-level integers.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SNumber
         ( -- * SNumber
           SNumber(N#, SNumber, unSNumber), SNumberRepr(..)

           -- ** Creation
         , snumber, trySNumber, unsafeUncheckedSNumber
         , unsafeMkSNumber, unsafeTryMkSNumber, unsafeUncheckedMkSNumber

           -- ** Existentials
         , SomeSNumber(..), someSNumberVal, withSNumber

           -- ** Comparison
         , SOrdering(..), compareSNumber, sameSNumber

           -- ** Arithmetic
         , OverflowArith(..)

           -- *** In 'Maybe'
         , tryAdd, trySub, tryMul

           -- *** Checked
         , UnrepresentableSNumber(..)
         , chkAdd, chkSub, chkMul

           -- ** Infallible
         , divExact

           -- ** Reification to Constraints
         , KnownSNumber(..), snumberVal
         , reifySNumber, reifySNumberAsNat

           -- * Miscellaneous
         , IntBits, IntMin, IntMaxP1
         , WordBits, WordMaxP1
         ) where

import Control.Exception (Exception, throw)
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts
         ( Word(W#), addWordC#, subWordC#, timesWord2#
         , Int(I#), addIntC#, subIntC#, mulIntMayOflo#, (*#)
         )
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeNats (type (^), KnownNat, Nat, SomeNat(..), someNatVal)
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)

import Data.SNumber.Internal
         ( NegativeReprUnsignedErr, OutOfReprRangeErr
         , IsAtLeastMinBound, IsLessThanMaxBound, ForbidNegZero
         )
import Kinds.Integer (CmpInteger, KnownInteger(..), pattern Pos, pattern Neg)
import qualified Kinds.Integer as K (Integer)
import Kinds.Num (type (+), type (-), type (*))

#include "MachDeps.h"

-- | @SNumber a n@ is a runtime representation of the value of @n@.
--
-- For @N# n :: SNumber a n@ and @N# m :: SNumber a m@, the following must hold:
--
-- * @n ~ m@ iff @n == m@ (according to the 'Eq' instance of @a@).
-- * @CmpInteger m n ~ 'LT@ iff @compare m n == LT@ (according to 'Ord').
-- * @CmpInteger m n ~ 'EQ@ iff @compare m n == EQ@.
-- * @CmpInteger m n ~ 'GT@ iff @compare m n == GT@.
-- * @toInteger n == integerVal \@n@
--
-- These are exactly the set of things we're willing to 'unsafeCoerce' proofs
-- for.  It is /unsafe/ to construct an 'SNumber' that violates these by any
-- means.
--
-- Note that the first requirement means that you must never construct
-- @N# 0 :: SNumber _ ('Neg 0)@, because that would prove that
-- @'Neg 0 ~ 'Pos 0@.  In practice, we largely ignore the existence of
-- @'Neg 0@: 'trySNumber' (and, by extension, the instance derivation via
-- 'KnownInteger') will throw a runtime error when trying to construct
-- @'Neg 0@, and 'SafeSNumber' instances explicitly exclude @'Neg 0@ with
-- type-level checks.
--
-- There are six main ways to introduce an 'SNumber', from the cartesian
-- product of several choices
--
-- * How do you validate that the value is in-bounds for the representation
--   type?
--
--     * If by runtime checks, the function name has a "try" infix.
--     * If by trusting the caller blindly, the function name has an "unsafe"
--       prefix and an "Unchecked" infix.
--     * If by type-level bounds checks, the function name has neither of these.
--
-- * How do you validate that the runtime Integer value matches the type-level
--   Integer?
--
--     * If by trusting the user to pass in the right Integer, the function has
--       an "unsafe" prefix and a \"Mk" infix: we're "making" the 'SNumber'
--       from a runtime value rather than deriving it from KnownNat.
--     * If by getting it from a KnownNat instance, the function name doesn't
--       have that infix.
--
-- Thus:
--
-- * 'snumber': type-level checks, safe @KnownNat@.
-- * 'trySNumber': runtime checks, safe @KnownNat@.
-- * 'unsafeUncheckedSNumber': no checks, safe @KnownNat@.
-- * 'unsafeMkSNumber': type-level checks, unsafe @Integer@ parameter.
-- * 'unsafeTryMkSNumber': runtime checks, unsafe @Integer@ parameter.
-- * 'unsafeUncheckedMkSNumber': no checks, unsafe @a@ parameter.  a.k.a. 'N#'.
--
-- Finally, there's one other way to get an 'SNumber': by asking the constraint
-- solver to give you one, with a 'KnownSNumber' class and its 'snumberVal'
-- method.  Currently, this will be solved automatically from 'KnownNat' by
-- using 'trySNumber', and of course it can be solved from a matching instance
-- in the function's context (which compiles to an @a@ parameter for
-- @KnownSNumber a n@).
newtype SNumber a (n :: K.Integer) = MkSNumber# a
  deriving newtype Show

-- Note: we must be extremely careful to prevent GHC from solving
-- @Data.Coerce.Coercible (SNumber m) (SNumber n)@.  Given two applications of
-- the same type constructor @X m@ and @X n@, there are three ways GHC might
-- solve this constraint:
-- * If the two types are equal (in our case, if @m ~ n@), the two types are
--   equal, and therefore GHC will consider the constraint solved.  This is
--   obviously fine, and we don't need to prevent it from happening.
-- * If both types are newtypes, whose constructors are in scope, and the two
--   underlying types are coercible, GHC will consider the constraint solved.
--   In our case, the first and third conditions are unavoidably satisfied, so
--   we must prevent the newtype constructor from ever being in scope.  We
--   achieve this (except for inside this module) by omitting it from the
--   export list.  Instead, we export its behavior as the pattern synonym 'N#'
--   and the selector 'unSNumber'.
-- * If each pair of parameters with nominal role is equal, and each pair of
--   parameters with representational role is coercible (note: parameters with
--   phantom role are ignored), GHC will consider the constraint solved.  In
--   our case, the only parameter involved is the 'Integer'; its inferred role
--   is phantom, which means all coercions over it would be solved; so we must
--   use a role annotation to change it.  The choice between representational
--   and nominal doesn't matter in practice, because GHC will never solve
--   @Coercible {Integer} m n@ anyway.  But we set it to nominal just because
--   that feels more right.

-- The 'Type' parameter needs to have nominal role, because coercing between
-- types with different Eq/Ord instances is unsafe.
--
-- See above on 'SNumber' for a description of the coercion behavior of the
-- 'Integer' parameter.
type role SNumber nominal nominal

-- | Create an 'SNumber' for @n@, with no safety measures whatsoever.
--
-- This pattern is identical to the newtype constructor of 'SNumber' except
-- that it isn't actualy a newtype constructor, so having it in scope doesn't
-- allow unsound coercions across 'SNumber' types.
pattern N# :: forall (n :: K.Integer) a. a -> SNumber a n
pattern N# {unSNumber} = MkSNumber# unSNumber
{-# COMPLETE N# #-}

data KnownSNumberDict a n = KnownSNumber a n => KnownSNumberDict

-- | Treat 'SNumber' as if it were a GADT containing a 'KnownSNumber' instance.
pattern SNumber
  :: forall (n :: K.Integer) a. SNumberRepr a => KnownSNumber a n => SNumber a n
pattern SNumber <-
  ((\x -> reifySNumber x (KnownSNumberDict @a @n)) -> KnownSNumberDict)
 where
  SNumber = snumberVal
{-# COMPLETE SNumber #-}

class LitIsAnything (n :: K.Integer)
instance ForbidNegZero n => LitIsAnything n

class LitIsUnsignedBits
        (w :: Nat)
        repr
        (a :: K.Integer -> Type)
        (n :: K.Integer)
instance ( IsAtLeastMinBound ('Pos 0) (NegativeReprUnsignedErr repr a) n
         , IsLessThanMaxBound
             ('Pos (2 ^ w))
             (OutOfReprRangeErr ('Pos 0) ('Pos (2 ^ w)) repr a)
             n
         , ForbidNegZero n
         )
      => LitIsUnsignedBits w repr a n

type SignedReprRangeErr w =
  OutOfReprRangeErr ('Neg (2 ^ (w - 1))) ('Pos (2 ^ (w - 1)))

class LitIsSignedBits (w :: Nat) repr (a :: K.Integer -> Type) (n :: K.Integer)
instance ( IsAtLeastMinBound
             ('Neg (2 ^ (w - 1)))
             (SignedReprRangeErr w repr a)
             n
         , IsLessThanMaxBound
             ('Pos (2 ^ (w - 1)))
             (SignedReprRangeErr w repr a)
             n
         , ForbidNegZero n
         )
      => LitIsSignedBits w repr a n

type LitIsNonNegative repr a =
  IsAtLeastMinBound ('Pos 0) (NegativeReprUnsignedErr repr a)

-- | The number of bits in the present system's 'Word' type.
type WordBits = WORD_SIZE_IN_BITS

-- | One more than the largest representable 'Word' value.
type WordMaxP1 = 'Pos (2 ^ WordBits)

-- | The number of bits in the present system's 'Int' type.
type IntBits = WORD_SIZE_IN_BITS

-- | The smallest representable 'Int' value.
type IntMin = 'Neg (2 ^ (IntBits - 1))

-- | One more than the largest representable 'Int' value.
type IntMaxP1 = 'Pos (2 ^ (IntBits - 1))

-- | The class of types that are suitable for use as integer value witnesses.
--
-- Implementing an instance of this class amounts to asserting that the type
-- and its 'Integral', 'Ord', and 'Eq' instances uphold the requirements of
-- 'SNumber', for any integer @n@ that satisfies @SafeSNumber a n@.
--
-- Furthermore, it requires that every value of @a@ is an integer, i.e. that
-- @forall x :: a. exists y :: Integer. x == fromInteger y, toInteger x == y@.
-- This ensures we can wrap any @a@ in @SomeSNumberVal@ and be sure it
-- corresponds to a valid @K.Integer@.
class Integral a => SNumberRepr a where
  -- | @SafeSNumber a n@ witnesses that @'N#' n@ is a valid @'SNumber' a n@.
  type SafeSNumber a :: K.Integer -> Constraint

instance SNumberRepr Int where
  type SafeSNumber Int = LitIsSignedBits IntBits Int (SNumber Int)

instance SNumberRepr Word where
  type SafeSNumber Word = LitIsUnsignedBits WordBits Word (SNumber Word)

instance SNumberRepr Integer where
  type SafeSNumber Integer = LitIsAnything

instance SNumberRepr Natural where
  type SafeSNumber Natural = LitIsNonNegative Natural (SNumber Natural)

-- | Construct an 'SNumber', doing all validation at the type level.
--
-- This is completely safe and cannot raise runtime errors.
snumber
  :: forall n a
   . (SNumberRepr a, SafeSNumber a n, KnownInteger n)
  => SNumber a n
snumber = unsafeMkSNumber (fromInteger (integerVal @n))

-- | Create an 'SNumber' for @n@, if it's faithfully represented by @a@.
--
-- This is completely safe, but it may raise runtime errors, because it tests
-- at runtime whether @a@ can represent @n@ correctly.
trySNumber
  :: forall n a
   . (SNumberRepr a, KnownInteger n)
  => Maybe (SNumber a n)
trySNumber = unsafeTryMkSNumber (integerVal @n)

-- | Create an 'SNumber' from a 'KnownInteger' constraint without any checks.
--
-- This will cause type unsoundness if the value is not correctly represented
-- by @a@ or if the instances of @a@ do not behave according to the safety
-- requirements described on 'SNumber'.
unsafeUncheckedSNumber :: forall n a. (Num a, KnownInteger n) => SNumber a n
unsafeUncheckedSNumber = unsafeUncheckedMkSNumber (fromInteger (integerVal @n))

-- | Semi-safely construct an 'SNumber' as if by 'N#'.
--
-- Callers must ensure that the @a@ argument is @fromInteger (integerVal @n)@
-- (semantically, not syntactically: @unsafeSNumber @('Pos 42) 42@ is fine and
-- is in fact the intended use case).
--
-- The extra constraint here compared to 'N#' ensures that if you write
-- @unsafeMkSNumber \@n (fromInteger n)@, the result is either a valid
-- 'SNumber' or a type error.  In particular, the cases this rules out are
-- those where @toInteger (fromInteger n :: a) /= n@ or where
-- @fromInteger m :: a == fromInteger n@ does not imply @m == n@.
unsafeMkSNumber :: forall n a. SafeSNumber a n => a -> SNumber a n
unsafeMkSNumber = N#

-- | Create an 'SNumber' for @n@, if it's faithfully represented by @a@.
--
-- As with 'unsafeMkSNumber', this trusts you to pass @integerVal \@n@, and
-- violating that will lead to type unsoundness.
--
-- This tests at runtime whether @a@ represents @n@ correctly.
unsafeTryMkSNumber
  :: forall n a. SNumberRepr a => Integer -> Maybe (SNumber a n)
unsafeTryMkSNumber x =
  let x' = fromIntegral x
  in  if toInteger x' /= x then Nothing else Just (N# x')
{-# INLINE unsafeTryMkSNumber #-}

-- | Create an 'SNumber' for @n@, with no safety measures whatsoever.
unsafeUncheckedMkSNumber :: forall n a. a -> SNumber a n
unsafeUncheckedMkSNumber = N#

-- | Like 'KnownNat', but represented by @a@ instead of 'Natural'.
--
-- This is currently solved automatically from 'KnownNat' via runtime checks,
-- to ease migration (we can incrementally strengthen 'KnownNat' constraints to
-- 'KnownSNumber' constraints), but in the future it may be changed to use
-- 'SafeSNumber', which will rule out runtime failures but will require
-- additional proofs in order to solve via 'KnownNat'.  For statically known
-- 'Integer's, these proofs will be entirely automatic.
--
-- This class is meant to be used primarily in instance contexts; for
-- standalone functions, it's probably better to pass an 'SNumber' explicitly.
class KnownSNumber a n where
  -- | Implementation of 'snumberVal'.
  --
  -- This has an inconvenient type variable order because it derives from the
  -- order they appear in the class head.
  snumberVal_ :: SNumber a n
  default snumberVal_ :: (SNumberRepr a, KnownInteger n) => SNumber a n
  snumberVal_ = fromMaybe (error "KnownSNumber: unrepresentable") trySNumber

-- TODO(awpr): GHC might helpfully simplify this to KnownInteger in inferred
-- type signatures, so we'll still end up passing Integers around and have
-- runtime type conversions?  But we only infer signatures for bindings in
-- let/where clauses, so presumably GHC will solve a 'KnownSNumber' using
-- either a 'KnownSNumber' or 'KnownInteger' instance that's already available,
-- thus doing the best we could do in context?
instance (SNumberRepr a, KnownInteger n) => KnownSNumber a n

-- | Get an 'SNumber' out of the context from 'KnownSNumber' or 'KnownNat'.
--
-- This has the number as its first type parameter, for TypeApplications
-- convenience.
snumberVal :: forall n a. KnownSNumber a n => SNumber a n
snumberVal = snumberVal_

-- TODO(awpr): Expose these functions as GEq/GOrd instances.

-- | Ordering results carrying evidence of type-level ordering relations.
data SOrdering m n where
  SLT :: CmpInteger m n ~ 'LT => SOrdering m n
  -- | This doesn't currently prove m ~ n, but since we've forbidden SNumbers
  -- of @'Neg 0@, it probably could.
  SEQ :: CmpInteger m n ~ 'EQ => SOrdering m n
  SGT :: CmpInteger m n ~ 'GT => SOrdering m n

-- | Compare two type-level 'Integer's using their runtime witnesses.
compareSNumber
  :: forall m n a. Ord a => SNumber a m -> SNumber a n -> SOrdering m n
compareSNumber (N# x) (N# y) = case compare x y of
  LT -> case unsafeCoerce Refl :: CmpInteger m n :~: 'LT of Refl -> SLT
  EQ -> case unsafeCoerce Refl :: CmpInteger m n :~: 'EQ of Refl -> SEQ
  GT -> case unsafeCoerce Refl :: CmpInteger m n :~: 'GT of Refl -> SGT

-- | Test equality of two type-level 'Integer's using their runtime witnesses.
sameSNumber :: Eq a => SNumber a m -> SNumber a n -> Maybe (m :~: n)
sameSNumber (N# x) (N# y)
  | x == y    = Just (unsafeCoerce Refl)
  | otherwise = Nothing

newtype c :=> a = CArr (c => a)

-- | Stash an 'SNumber' at the type level as a 'KnownSNumber' instance.
reifySNumber :: forall a n r. SNumber a n -> (KnownSNumber a n => r) -> r
reifySNumber n r = f n
 where
  f :: SNumber a n -> r
  f = unsafeCoerce (CArr r :: KnownSNumber a n :=> r)

-- | Use a positive 'SNumber' to introduce a 'KnownNat' instance.
reifySNumberAsNat
  :: forall n r a
   . Integral a
  => SNumber a ('Pos n) -> (KnownNat n => r) -> r
reifySNumberAsNat (N# x) r = case someNatVal (fromIntegral x) of
  SomeNat (_ :: Proxy m) -> case unsafeCoerce Refl :: n :~: m of Refl -> r

-- | An 'SNumber' with an existential 'K.Integer' type parameter.
data SomeSNumber a = forall n. SomeSNumber (SNumber a n)

-- | Create an @'SNumber' a@ from any value of type @a@.
--
-- Since 'SNumberRepr' guarantees every @a@ value is an integer, we can freely
-- wrap up a value in an 'SNumber' with existential 'K.Integer' type parameter.
someSNumberVal :: SNumberRepr a => a -> SomeSNumber a
someSNumberVal x = withSNumber x SomeSNumber

-- | Like 'someSNumberVal', but in quantified CPS style rather than GADT style.
withSNumber :: SNumberRepr a => a -> (forall n. SNumber a n -> r) -> r
withSNumber x r = r (N# x)

-- | Arithmetic ops with overflow detection.
--
-- Laws:
--
-- * @overflowAdd x y = (False, xy) =>
--      toInteger xy === toInteger x + toInteger y@
-- * @overflowSub x y = (False, xy) =>
--      toInteger xy === toInteger x - toInteger y@
-- * @overflowMul x y = (False, xy) =>
--      toInteger xy === toInteger x * toInteger y@
--
-- This is used for arithmetic on 'SNumber' runtime values, so creating
-- incorrect instances is type-unsafe.
class Integral a => OverflowArith a where
  overflowAdd :: a -> a -> (Bool, a)
  overflowSub :: a -> a -> (Bool, a)
  overflowMul :: a -> a -> (Bool, a)

instance OverflowArith Word where
  overflowAdd (W# x) (W# y) =
    case addWordC# x y of (# xy, ovf #) -> (I# ovf /= 0, W# xy)
  overflowSub (W# x) (W# y) =
    case subWordC# x y of (# xy, ovf #) -> (I# ovf /= 0, W# xy)
  overflowMul (W# x) (W# y) =
    case timesWord2# x y of (# hi, lo #) -> (W# hi /= 0, W# lo)

instance OverflowArith Int where
  overflowAdd (I# x) (I# y) =
    case addIntC# x y of (# xy, ovf #) -> (I# ovf /= 0, I# xy)
  overflowSub (I# x) (I# y) =
    case subIntC# x y of (# xy, ovf #) -> (I# ovf /= 0, I# xy)
  overflowMul (I# x) (I# y) =
    -- TODO(awpr): Newer versions of base have 'timesInt2#'; consider using CPP
    -- to use that when available.
    let xy = I# (x *# y)
    in  case mulIntMayOflo# x y of
          ovf -> if I# ovf /= 0
            then (toInteger xy /= toInteger (I# x) * toInteger (I# y), xy)
            else (False, I# (x *# y))

instance OverflowArith Natural where
  overflowAdd x y = (False, x + y)
  overflowSub x y = (x > y, x - y)
  overflowMul x y = (False, x * y)

instance OverflowArith Integer where
  overflowAdd x y = (False, x + y)
  overflowSub x y = (False, x - y)
  overflowMul x y = (False, x * y)

-- Note: this internal helper function will make any SNumber result you want,
-- thus "unsafe".
unsafeMkTry
  :: (a -> a -> (Bool, a)) -> SNumber a n -> SNumber a m -> Maybe (SNumber a o)
unsafeMkTry f (N# x) (N# y) = case f x y of
  (True, _) -> Nothing
  (False, xy) -> Just (N# xy)

-- | Compute a runtime witness of @m + n@, or 'Nothing'.
tryAdd
  :: (SNumberRepr a, OverflowArith a)
  => SNumber a m -> SNumber a n -> Maybe (SNumber a (m + n))
tryAdd = unsafeMkTry overflowAdd

-- | Compute a runtime witness of @m - n@, or 'Nothing'.
trySub
  :: (SNumberRepr a, OverflowArith a)
  => SNumber a m -> SNumber a n -> Maybe (SNumber a (m - n))
trySub = unsafeMkTry overflowSub

-- | Compute a runtime witness of @m * n@, or 'Nothing'.
tryMul
  :: (SNumberRepr a, OverflowArith a)
  => SNumber a m -> SNumber a n -> Maybe (SNumber a (m * n))
tryMul = unsafeMkTry overflowMul

data UnrepresentableSNumber = UnrepresentableSNumber String Integer Integer
  deriving Show

instance Exception UnrepresentableSNumber

-- Note: this internal helper function will make any SNumber result you want,
-- thus "unsafe".
unsafeMkChk
  :: (HasCallStack, Integral a)
  => String
  -> (a -> a -> (Bool, a)) -> SNumber a n -> SNumber a m -> SNumber a o
unsafeMkChk s f (N# x) (N# y) = case f x y of
  (True, _) -> throw (UnrepresentableSNumber s (toInteger x) (toInteger y))
  (False, xy) -> N# xy

-- | Compute a runtime witness of @m + n@, or throw.
chkAdd
  :: (SNumberRepr a, OverflowArith a, HasCallStack)
  => SNumber a m -> SNumber a n -> SNumber a (m + n)
chkAdd = withFrozenCallStack unsafeMkChk "+" overflowAdd

-- | Compute a runtime witness of @m - n@, or throw.
chkSub
  :: (SNumberRepr a, OverflowArith a, HasCallStack)
  => SNumber a m -> SNumber a n -> SNumber a (m - n)
chkSub = withFrozenCallStack unsafeMkChk "-" overflowSub

-- | Compute a runtime witness of @m * n@, or throw.
chkMul
  :: (SNumberRepr a, OverflowArith a, HasCallStack)
  => SNumber a m -> SNumber a n -> SNumber a (m * n)
chkMul = withFrozenCallStack unsafeMkChk "*" overflowMul

-- | Compute a runtime witness of exact division.
--
-- We could provide division in terms of 'GHC.TypeNats.Div' instead, but
-- "Kinds.Integer" doesn't currently have division.
divExact :: SNumberRepr a => SNumber a (m * n) -> SNumber a n -> SNumber a m
divExact = unsafeMkChk "/" (\x y -> (False, x `div` y))
