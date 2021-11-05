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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- We hid all uses of this from HLint
#if !defined(__HLINT__)
{-# LANGUAGE StandaloneDeriving #-}
#endif

-- | The library component of "DependentLiterals.Plugin".
--
-- This provides a class for dependent numeric literal functionality, the entry
-- points used by the plugin for literals and patterns, and a few ways of
-- defining instances with less unsafe-ty.

module DependentLiterals.Int
         ( -- * Dependent Literals
           HasIntLiterals(..), valueOf

           -- ** Convenience Aliases
         , HasBasicLiterals, AllowsIntLiteral, IntLiteral

           -- ** Safer Implementations
         , StockLit(..)
         , SNum(..), Satisfying(..), SNumLit(..)
         , type (-#), CMaybe(..)

           -- ** Plugin Entry Points
         , lit#, match#

           -- * Implementation Details
         , NoAssertion
         ) where

import Data.Functor.Const (Const(..))
import Data.Functor.Identity (Identity(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import Data.Ratio (Ratio)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Semigroup (Sum(..), Product(..), Min(..), Max(..))
import Data.Type.Equality ((:~:)(..))
import Foreign.C.Types
         ( CChar, CSChar, CUChar, CWchar
         , CShort, CUShort, CInt, CUInt, CLong, CULong, CLLong, CULLong
         , CPtrdiff, CSize, CSigAtomic, CBool
         , CIntPtr, CUIntPtr, CIntMax, CUIntMax
         , CClock, CTime, CUSeconds, CSUSeconds
         )
import GHC.TypeLits (TypeError, ErrorMessage(..))
import GHC.TypeNats (Nat)
import Numeric.Natural (Natural)
import Unsafe.Coerce (unsafeCoerce)

import Data.SInt (SInt(SI#, unSInt))
import Data.SNumber (SafeSNumber, SNumber(N#), sameSNumber, unsafeMkSNumber)
import Data.Tagged (Tagged(..))
import Data.Wrapped (Wrapped(..))
import Kinds.Integer (type (-#), KnownInteger(..), pattern Pos)
import Kinds.Num (ToInteger)
import Kinds.Ord (type (>=), type (<))
import qualified Kinds.Integer as K (Integer)
import Data.Fin.Int (Fin, finToInt, unsafeFin)

import DependentLiterals.Bounds
         ( CheckAtLeastMinBound, CheckLessThanMaxBound
         , AssertEq, OutOfRangeMsg, ShowTypedNum
         )


-- | A GADT containing some @t x@ along with a @c x@ instance.
data Satisfying c t = forall x. c x => Satisfying (t x)

-- | 'Maybe' on 'Constraint's.
data CMaybe c = c => CJust | CNothing

-- | A user-facing dependent numeric typeclass.
--
-- This gives an isomorphism between @a@ and @exists n. SNumber (SNumRepr a) n@
-- given that @SNumConstraint a n@ holds.
--
-- To get instances of 'HasIntLiterals' without interacting with the unsafe
-- parts of the library, implement this class and lift it to 'HasIntLiterals'
-- with @deriving HasIntLiterals via SNumLit MyAssertionClass MyType@.
class SNum a where
  -- | The underlying numerical representation type.
  type SNumRepr a :: Type

  -- | The constraint on literal values to validate/refine @a@.
  type SNumConstraint a :: K.Integer -> Constraint

  -- | Conversion from 'SNumber' with a proof of 'SNumConstraint' into @a@.
  --
  -- Used for interpreting integral literals.
  fromSNum :: Satisfying (SNumConstraint a) (SNumber (SNumRepr a)) -> a

  -- | Conversion from @a@ into 'SNumber' with a proof of 'SNumConstraint.'
  --
  -- Used for providing proofs on successful pattern matches.
  intoSNum :: a -> Satisfying (SNumConstraint a) (SNumber (SNumRepr a))

instance SNum (SInt n) where
  type SNumRepr (SInt n) = Int
#if !defined(__HLINT__)
  type SNumConstraint (SInt n) = (~) ('Pos n)
#endif
  fromSNum (Satisfying (N# x)) = SI# x
  intoSNum x = Satisfying (N# @('Pos n) (unSInt x))

instance SNum (SNumber a n) where
  type SNumRepr (SNumber a n) = a
#if !defined(__HLINT__)
  type SNumConstraint (SNumber a n) = (~) n
#endif
  fromSNum (Satisfying x) = x
  intoSNum = Satisfying

class (n < 'Pos m, n >= 'Pos 0)
   => FinInBounds (m :: Nat) (n :: K.Integer)
instance (n < 'Pos m, n >= 'Pos 0) => FinInBounds m n

instance SNum (Fin n) where
  type SNumRepr (Fin n) = Int
  type SNumConstraint (Fin n) = FinInBounds n
  fromSNum (Satisfying (N# x)) = unsafeFin x
  intoSNum x = unsafeCoerce (Satisfying @NoConstraint (N# (finToInt x)))

-- | The main class providing dependent-literals functionality.
--
-- Instances of this can have integral literals and numeric patterns with
-- @-fplugin="DependentLiterals.Plugin"@.
--
-- This class exposes the internal unsafe machinery of the library; a safer and
-- less error-prone way to get instances for it is via 'SNum' or 'StockLit'.
class HasIntLiterals a where
  -- | Constraint for representational validity of a literal: this is meant to
  -- prevent overflowed literals from being wrapped incorrectly in 'N#'.  This
  -- is required both for expressions and for patterns.
  --
  -- Safety contract: 'unsafeFromInteger' and 'unsafeMatchInteger' must not
  -- construct illegal @SNumber n@ values when called with @Tagged \@n n@; they
  -- may use 'ReprAssertion' to restrict the values they can receive.
  type ReprAssertion a :: Type -> K.Integer -> Constraint

  -- | @LitConstraint a n@ constrains or refines @a@ given the literal value.
  --
  -- This is what's proven about the integer value and type by matching on a
  -- numeric pattern; for example, in the case of @SInt n@, @LitConstraint a m@
  -- is @m ~ n@, so that matching numeric 'SInt' patterns introduces equality
  -- proofs for the type parameter.
  type LitConstraint a :: K.Integer -> Constraint

  -- | Like 'LitConstraint' but with pretty error messages.
  --
  -- This is used on integral literals to provide custom error messages for
  -- failed constraints.
  type LitAssertion a :: Type -> K.Integer -> Constraint

  -- | Runtime conversion from 'Integer' to the appropriate type.
  --
  -- Unsafe in that it trusts that the 'Integer' you give it is the same as the
  -- type-level one.
  unsafeFromInteger
    :: forall n b
     . (LitAssertion a b n, ReprAssertion a b n)
    => Proxy b -> Tagged n Integer -> a

  -- | Runtime pattern match implementation.
  --
  -- Unsafe in that it trusts that the 'Integer' you give it is the same as the
  -- type-level one.
  unsafeMatchInteger
    :: forall n b
     . ReprAssertion a b n
    => Proxy b -> Tagged n Integer -> a -> CMaybe (LitConstraint a n)

-- | A constraint alias that asserts you can use any integral literal.
--
-- This can be useful in polymorphic contexts when you don't want to list out
-- constraints for every literal value you need, and are willing to accept that
-- some types with stronger compile-time validation will be excluded.
type HasBasicLiterals a =
  ( HasIntLiterals a
  , LitAssertion a ~ NoAssertion
  , ReprAssertion a ~ NoAssertion
  )

-- | A constraint alias showing that the particular value @n@ is valid for @a@.
--
-- With this in context along with @'HasIntLiterals' a@, you can use an
-- integral literal value @n@ at type @a@.  See also 'IntLiteral'.
type AllowsIntLiteral n a =
  ( LitAssertion a a (ToInteger n)
  , ReprAssertion a a (ToInteger n)
  )

-- | A convenient form of 'IntLiteral' when only one value is needed.
--
-- This is a constraint tuple, so using this multiple times in a signature
-- creates a bit of constraint pollution; for tidier signatures, use one
-- 'HasIntLiterals' and several 'AllowsIntLiteral's.
type IntLiteral n a = (HasIntLiterals a, AllowsIntLiteral n a)

-- | The unsafe entry point used by "DependentLiterals.Plugin" for literals.
--
-- This is unsafe to use explicitly, since it implicitly trusts that the given
-- 'Integer' is equal to the type-level integer.  The plugin guarantees this
-- itself when generating calls, so its uses are safe.
lit#
  :: forall n a
   . (HasIntLiterals a, ReprAssertion a a n, LitAssertion a a n)
  => (Num a => a) -> Integer -> a
lit# _ = unsafeFromInteger (Proxy @a) . Tagged @n

-- | The unsafe entry point used by "DependentLiterals.Plugin" for patterns.
--
-- This is unsafe to use explicitly, since it implicitly trusts that the given
-- 'Integer' is equal to the type-level integer.  The plugin guarantees this
-- itself when generating calls, so its uses are safe.
match#
  :: forall n a
   . (HasIntLiterals a, ReprAssertion a a n)
  => (Num a => a) -> Integer -> a -> CMaybe (LitConstraint a n)
match# _ = unsafeMatchInteger (Proxy @a) . Tagged @n

-- | 'valueOf' specialized to 'Integer'.
valueOf' :: forall n a. (IntLiteral n a, KnownInteger n) => a
valueOf' = unsafeFromInteger (Proxy @a) (Tagged @n $ toInteger $ integerVal @n)

-- | Get the value of a type-level number at runtime, as if it were a literal.
--
-- That is, when DependentLiterals is enabled, @42@ and @valueOf \@42@ are the
-- same thing.  (When it's not enabled, @42@ is just @fromInteger 42@).
valueOf
  :: forall n a
   . (IntLiteral (ToInteger n) a, KnownInteger (ToInteger n))
  => a
valueOf = valueOf' @(ToInteger n)

-- | A newtype carrying a 'HasIntLiterals' instance for use with @DerivingVia@.
newtype SNumLit (c :: Type -> K.Integer -> Constraint) a = SNumLit a

class SNumConstraint a n => SNumLitAssertion c a b n
#if !defined(__HLINT__)
instance (c b n, cc ~ SNumConstraint a, forall m. c b m => cc m)
      => SNumLitAssertion c a b n
#endif

class SafeSNumber repr n => SNumReprAssertion repr b n
instance SafeSNumber repr n => SNumReprAssertion repr b n

instance (Eq (SNumRepr a), Num (SNumRepr a), SNum a)
      => HasIntLiterals (SNumLit c a) where
  type ReprAssertion (SNumLit c a) = SNumReprAssertion (SNumRepr a)
  type LitAssertion (SNumLit c a) = SNumLitAssertion c a
  type LitConstraint (SNumLit c a) = SNumConstraint a

  unsafeFromInteger (_ :: Proxy b) (Tagged n :: Tagged n Integer) =
    SNumLit $ fromSNum $ Satisfying (unsafeMkSNumber @n (fromInteger n))

  unsafeMatchInteger (_ :: Proxy b) (Tagged n :: Tagged n Integer) (SNumLit a) =
    case intoSNum a of
      Satisfying m ->
        case sameSNumber (unsafeMkSNumber @n (fromInteger n)) m of
          Just Refl -> CJust
          Nothing   -> CNothing


-- | Use @'Wrapped' Num@ directly instead.
--
-- The 'HasIntLiterals' instance of @'Wrapped' Num@ results in integral
-- literals and patterns that behave like the stock literals would have,
-- according to the underlying type's 'Num' instance.
--
-- For use with @-XDerivingVia@.  This calls through to the 'fromIntegral' of
-- the underlying 'Num' instance for the final conversion.  If the type in
-- question has a 'Num' instance and it's acceptable for literals to overflow
-- (or if the type is supported by -Woverflowed-literals), this is a good way
-- to get an instance.
--
-- This is suitable for @DerivingVia@ and tends to work as a deriving clause,
-- so:
--
-- @
--     newtype MyType = MyType Int
--       deriving (Eq, Num)
--       deriving HasIntLiterals via Wrapped Num MyType
-- @
--
-- Note in this case you could just as well say @deriving HasIntLiterals@ to
-- get a @GeneralizedNewtypeDeriving@ instance that consumes Int literals and
-- coerces them, but if you wrote a custom Num instance, @via Wrapped Num MyType@
-- will respect it.
newtype StockLit a = StockLit a
  deriving HasIntLiterals via Wrapped Num a

class NoConstraint (a :: k)
instance NoConstraint a

-- | The "assertion" used by basic integral literals, which is always solvable.
class NoAssertion (a :: Type) (n :: K.Integer)
instance NoAssertion a n

instance (Eq a, Num a) => HasIntLiterals (Wrapped Num a) where
  type ReprAssertion (Wrapped Num a) = NoAssertion
  type LitConstraint (Wrapped Num a) = NoConstraint
  type LitAssertion (Wrapped Num a) = NoAssertion

  unsafeFromInteger _ (Tagged n) = Wrapped (fromInteger n)

  unsafeMatchInteger _ (Tagged n) (Wrapped a) = if fromInteger n == a
    then CJust
    else CNothing

class n ~ m => SIntLitAssertion (m :: K.Integer) (a :: Type) (n :: K.Integer)
instance AssertEq
           (TypeError
             ('Text "SInt literal value does not match " ':<>:
              'Text "expected type index:" ':$$:
              'Text "  " ':<>: ShowTypedNum a n))
           n
           m
      => SIntLitAssertion m a n

class (n >= 'Pos 0, n < 'Pos m) => FinLitAssertion m a n
instance ( CheckLessThanMaxBound
             (OutOfRangeMsg ('Pos 0) ('Pos m) a n)
             ('Pos m)
             a
             n
         , CheckAtLeastMinBound
             (OutOfRangeMsg ('Pos 0) ('Pos m) a n)
             ('Pos 0)
             a
             n
         )
      => FinLitAssertion m a n

-- HLint doesn't understand any of these because of DerivingVia :/
#if !defined(__HLINT__)
deriving via SNumLit (SIntLitAssertion ('Pos n)) (SInt n)
  instance HasIntLiterals (SInt n)

deriving via SNumLit (SIntLitAssertion n) (SNumber a n)
  instance (Eq a, Num a) => HasIntLiterals (SNumber a n)

deriving via SNumLit (FinLitAssertion n) (Fin n) instance HasIntLiterals (Fin n)

deriving via Wrapped Num Integer instance HasIntLiterals Integer
deriving via Wrapped Num Natural instance HasIntLiterals Natural
deriving via Wrapped Num (Ratio a)
  instance Integral a => HasIntLiterals (Ratio a)

deriving via Wrapped Num Int instance HasIntLiterals Int
deriving via Wrapped Num Int8 instance HasIntLiterals Int8
deriving via Wrapped Num Int16 instance HasIntLiterals Int16
deriving via Wrapped Num Int32 instance HasIntLiterals Int32
deriving via Wrapped Num Int64 instance HasIntLiterals Int64

deriving via Wrapped Num Word instance HasIntLiterals Word
deriving via Wrapped Num Word8 instance HasIntLiterals Word8
deriving via Wrapped Num Word16 instance HasIntLiterals Word16
deriving via Wrapped Num Word32 instance HasIntLiterals Word32
deriving via Wrapped Num Word64 instance HasIntLiterals Word64

deriving via Wrapped Num Float instance HasIntLiterals Float
deriving via Wrapped Num Double instance HasIntLiterals Double

deriving via Wrapped Num (Sum a)
  instance (Eq a, Num a) => HasIntLiterals (Sum a)
deriving via Wrapped Num (Product a)
  instance (Eq a, Num a) => HasIntLiterals (Product a)
deriving via Wrapped Num (Min a)
  instance (Eq a, Num a) => HasIntLiterals (Min a)
deriving via Wrapped Num (Max a)
  instance (Eq a, Num a) => HasIntLiterals (Max a)
deriving via Wrapped Num (Const a b)
  instance (Eq a, Num a) => HasIntLiterals (Const a b)
deriving via Wrapped Num (Identity a)
  instance (Eq a, Num a) => HasIntLiterals (Identity a)

deriving via Wrapped Num CChar instance HasIntLiterals CChar
deriving via Wrapped Num CSChar instance HasIntLiterals CSChar
deriving via Wrapped Num CUChar instance HasIntLiterals CUChar
deriving via Wrapped Num CShort instance HasIntLiterals CShort
deriving via Wrapped Num CUShort instance HasIntLiterals CUShort
deriving via Wrapped Num CInt instance HasIntLiterals CInt
deriving via Wrapped Num CUInt instance HasIntLiterals CUInt
deriving via Wrapped Num CLong instance HasIntLiterals CLong
deriving via Wrapped Num CULong instance HasIntLiterals CULong
deriving via Wrapped Num CPtrdiff instance HasIntLiterals CPtrdiff
deriving via Wrapped Num CSize instance HasIntLiterals CSize
deriving via Wrapped Num CWchar instance HasIntLiterals CWchar
deriving via Wrapped Num CSigAtomic instance HasIntLiterals CSigAtomic
deriving via Wrapped Num CLLong instance HasIntLiterals CLLong
deriving via Wrapped Num CULLong instance HasIntLiterals CULLong
deriving via Wrapped Num CBool instance HasIntLiterals CBool
deriving via Wrapped Num CIntPtr instance HasIntLiterals CIntPtr
deriving via Wrapped Num CUIntPtr instance HasIntLiterals CUIntPtr
deriving via Wrapped Num CIntMax instance HasIntLiterals CIntMax
deriving via Wrapped Num CUIntMax instance HasIntLiterals CUIntMax
deriving via Wrapped Num CClock instance HasIntLiterals CClock
deriving via Wrapped Num CTime instance HasIntLiterals CTime
deriving via Wrapped Num CUSeconds instance HasIntLiterals CUSeconds
deriving via Wrapped Num CSUSeconds instance HasIntLiterals CSUSeconds
#endif
