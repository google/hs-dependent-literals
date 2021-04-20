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

module DependentLiterals.Int
         ( type (-#), CMaybe(..)
         , HasBasicLiterals, AllowsIntLiteral, IntLiteral
         , StockLit(..), SNum(..), SNumLit(..)

         , lit#, match#, valueOf, HasIntLiterals(..)
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

import Data.Tagged (Tagged(..))
import Data.SInt (SInt(SI#, unSInt))
import Data.SNumber (SafeSNumber, SNumber(N#), sameSNumber, unsafeMkSNumber)
import Kinds.Integer (type (-#), KnownInteger(..), pattern Pos)
import Kinds.Num (type (>=), type (<), Cmp, ToInteger)
import qualified Kinds.Integer as K (Integer)
import Data.Fin.Int (Fin, finToInt, unsafeFin)

import DependentLiterals.Bounds
         ( CheckAtLeastMinBound, CheckLessThanMaxBound
         , AssertEq, OutOfRangeMsg, ShowTypedNum
         )


data Satisfying c t = forall x. c x => Satisfying (t x)
data CMaybe c = c => CJust | CNothing

-- User-facing dependent numeric type class: implement this small, clean API
-- and lift it to HasIntLiterals with DerivingVia + SNumLit.
class SNum a where
  type SNumRepr a :: Type
  type SNumConstraint a :: K.Integer -> Constraint
  fromSNum :: Satisfying (SNumConstraint a) (SNumber (SNumRepr a)) -> a
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

class (Cmp n ('Pos m) ~ 'LT, n >= 'Pos 0)
   => FinInBounds (m :: Nat) (n :: K.Integer)
instance (Cmp n ('Pos m) ~ 'LT, n >= 'Pos 0) => FinInBounds m n

instance SNum (Fin n) where
  type SNumRepr (Fin n) = Int
  type SNumConstraint (Fin n) = FinInBounds n
  fromSNum (Satisfying (N# x)) = unsafeFin x
  intoSNum x = unsafeCoerce (Satisfying @NoConstraint (N# (finToInt x)))

class HasIntLiterals a where
  -- Constraint for representational validity of a literal: this is meant to
  -- prevent overflowed literals from being wrapped incorrectly in 'N#'.  This
  -- is required both for expressions and for patterns.
  --
  -- Safety contract: 'unsafeFromInteger' and 'unsafeMatchInteger' must not
  -- construct illegal @SNumber n@ values when called with @Tagged \@n n@; they
  -- may use 'ReprAssertion' to restrict the values they can receive.
  type ReprAssertion a :: Type -> K.Integer -> Constraint

  type LitConstraint a :: K.Integer -> Constraint

  -- Like LitConstraint but with pretty error messages.
  type LitAssertion a :: Type -> K.Integer -> Constraint

  -- Unsafe in that it trusts that the Integer you give it is the same as the
  -- type-level one.
  unsafeFromInteger
    :: forall n b
     . (LitAssertion a b n, ReprAssertion a b n)
    => Proxy b -> Tagged n Integer -> a

  -- Unsafe in that it trusts that the Integer you give it is the same as the
  -- type-level one.
  unsafeMatchInteger
    :: forall n b
     . ReprAssertion a b n
    => Proxy b -> Tagged n Integer -> a -> CMaybe (LitConstraint a n)

type HasBasicLiterals a =
  ( HasIntLiterals a
  , LitAssertion a ~ NoAssertion
  , ReprAssertion a ~ NoAssertion
  )

type AllowsIntLiteral n a =
  ( LitAssertion a a (ToInteger n)
  , ReprAssertion a a (ToInteger n)
  )

type IntLiteral n a = (HasIntLiterals a, AllowsIntLiteral n a)

lit#
  :: forall n a
   . (HasIntLiterals a, ReprAssertion a a n, LitAssertion a a n)
  => (Num a => a) -> Integer -> a
lit# _ = unsafeFromInteger (Proxy @a) . Tagged @n

match#
  :: forall n a
   . (HasIntLiterals a, ReprAssertion a a n)
  => (Num a => a) -> Integer -> a -> CMaybe (LitConstraint a n)
match# _ = unsafeMatchInteger (Proxy @a) . Tagged @n

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


-- | A type with a 'HasIntLiterals' instance that just behaves like 'Num'.
--
-- For use with @-XDerivingVia@.  This calls through to the 'fromIntegral' of
-- the underlying 'Num' instance for the final conversion.  If the type in
-- question has a 'Num' instance and it's acceptable for literals to overflow
-- (or if the type is supported by -Woverflowed-literals), this is a good way
-- to get an instance.
--
-- This is suitable for DerivingVia and tends to work as a deriving clause, so:
--
--     newtype MyType = MyType Int
--       deriving Num
--       deriving HasIntLiterals via StockLit Int
--
-- Note in this case you could just as well say @deriving HasIntLiterals@ to
-- get a @GeneralizedNewtypeDeriving@ instance that consumes Int literals and
-- coerces them, but if you wrote a custom Num instance, @via FromIntegral _ _@
-- will respect it.
newtype StockLit a = StockLit a

class NoConstraint (a :: k)
instance NoConstraint a

class NoAssertion (a :: Type) (n :: K.Integer)
instance NoAssertion a n

instance (Eq a, Num a) => HasIntLiterals (StockLit a) where
  type ReprAssertion (StockLit a) = NoAssertion
  type LitConstraint (StockLit a) = NoConstraint
  type LitAssertion (StockLit a) = NoAssertion

  unsafeFromInteger _ (Tagged n) = StockLit (fromInteger n)

  unsafeMatchInteger _ (Tagged n) (StockLit a) = if fromInteger n == a
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

deriving via StockLit Integer instance HasIntLiterals Integer
deriving via StockLit Natural instance HasIntLiterals Natural
deriving via StockLit (Ratio a) instance Integral a => HasIntLiterals (Ratio a)

deriving via StockLit Int instance HasIntLiterals Int
deriving via StockLit Int8 instance HasIntLiterals Int8
deriving via StockLit Int16 instance HasIntLiterals Int16
deriving via StockLit Int32 instance HasIntLiterals Int32
deriving via StockLit Int64 instance HasIntLiterals Int64

deriving via StockLit Word instance HasIntLiterals Word
deriving via StockLit Word8 instance HasIntLiterals Word8
deriving via StockLit Word16 instance HasIntLiterals Word16
deriving via StockLit Word32 instance HasIntLiterals Word32
deriving via StockLit Word64 instance HasIntLiterals Word64

deriving via StockLit Float instance HasIntLiterals Float
deriving via StockLit Double instance HasIntLiterals Double

deriving via StockLit (Sum a)
  instance (Eq a, Num a) => HasIntLiterals (Sum a)
deriving via StockLit (Product a)
  instance (Eq a, Num a) => HasIntLiterals (Product a)
deriving via StockLit (Min a)
  instance (Eq a, Num a) => HasIntLiterals (Min a)
deriving via StockLit (Max a)
  instance (Eq a, Num a) => HasIntLiterals (Max a)
deriving via StockLit (Const a b)
  instance (Eq a, Num a) => HasIntLiterals (Const a b)
deriving via StockLit (Identity a)
  instance (Eq a, Num a) => HasIntLiterals (Identity a)

deriving via StockLit CChar instance HasIntLiterals CChar
deriving via StockLit CSChar instance HasIntLiterals CSChar
deriving via StockLit CUChar instance HasIntLiterals CUChar
deriving via StockLit CShort instance HasIntLiterals CShort
deriving via StockLit CUShort instance HasIntLiterals CUShort
deriving via StockLit CInt instance HasIntLiterals CInt
deriving via StockLit CUInt instance HasIntLiterals CUInt
deriving via StockLit CLong instance HasIntLiterals CLong
deriving via StockLit CULong instance HasIntLiterals CULong
deriving via StockLit CPtrdiff instance HasIntLiterals CPtrdiff
deriving via StockLit CSize instance HasIntLiterals CSize
deriving via StockLit CWchar instance HasIntLiterals CWchar
deriving via StockLit CSigAtomic instance HasIntLiterals CSigAtomic
deriving via StockLit CLLong instance HasIntLiterals CLLong
deriving via StockLit CULLong instance HasIntLiterals CULLong
deriving via StockLit CBool instance HasIntLiterals CBool
deriving via StockLit CIntPtr instance HasIntLiterals CIntPtr
deriving via StockLit CUIntPtr instance HasIntLiterals CUIntPtr
deriving via StockLit CIntMax instance HasIntLiterals CIntMax
deriving via StockLit CUIntMax instance HasIntLiterals CUIntMax
deriving via StockLit CClock instance HasIntLiterals CClock
deriving via StockLit CTime instance HasIntLiterals CTime
deriving via StockLit CUSeconds instance HasIntLiterals CUSeconds
deriving via StockLit CSUSeconds instance HasIntLiterals CSUSeconds
#endif
