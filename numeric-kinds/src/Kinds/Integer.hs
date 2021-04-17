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

-- | Type-level 'Integer'.
--
-- Import Integer qualified as K and refer to it as K.Integer.
--
-- It turns out this is approximately identical to an in-progress GHC change
-- https://github.com/ghc-proposals/ghc-proposals/pull/154, right down to the
-- particulars of how to encode kind-classes.  Apparently I was on the right
-- track!  TODO(awpr): when that thing is merged and readily available, remove
-- this or turn it into a compat package.
--
-- Once that proposal is complete, we can migrate by replacing K.Integer with
-- just Integer.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Kinds.Integer
         ( -- * Type-level Integers
           Integer(..), ToInteger

           -- ** Runtime Values
         , KnownInteger(..)

           -- ** Specialized Arithmetic and Comparison
         , AddInteger, SubInteger, CmpInteger
         , type (-#)

           -- * Utility
         , CaseOrdering
         ) where

import Prelude hiding (Integer)
import qualified Prelude as P
import GHC.Exts (proxy#)
import GHC.TypeNats (KnownNat, Nat, natVal')

import Kinds.Num (type (+), type (-), Cmp, FromNat, ToInteger)

-- | Type-level signed numbers
data Integer = Pos Nat | Neg Nat

-- | Mostly internal; the "snumber" package provides more useful functionality.
class KnownInteger (n :: Integer) where
  integerVal :: P.Integer

instance KnownNat n => KnownInteger ('Pos n) where
  integerVal = toInteger $ natVal' @n proxy#

instance KnownNat n => KnownInteger ('Neg n) where
  integerVal =
    let x = natVal' @n proxy#
    in  if x == 0
          then error "illegal KnownInteger (-0)"
          else negate $ toInteger x

type instance Cmp {- k=Integer -} x y = CmpInteger x y
type instance FromNat {- k=Integer -} n = 'Pos n

type instance ToInteger {- k=Integer -} n = n

type instance x + y = AddInteger x y

type instance x - y = SubInteger x y

-- | Comparison of type-level integers.
type family CmpInteger m n where
  CmpInteger ('Pos m) ('Pos n) = Cmp m n
  CmpInteger ('Pos 0) ('Neg 0) = 'EQ
  CmpInteger ('Pos m) ('Neg n) = 'GT
  CmpInteger ('Neg 0) ('Pos 0) = 'EQ
  CmpInteger ('Neg m) ('Pos n) = 'LT
  CmpInteger ('Neg m) ('Neg n) = Cmp n m -- Note: reversed

-- | Type-level eliminator for 'Ordering'.
--
-- @CaseOrdering o lt eq gt@ selects from among @lt@, @eq@, and @gt@ according
-- to @o@.  This is primarily exported so Haddock doesn't complain about being
-- unable to link it when it's mentioned in type instance clauses.
type family CaseOrdering (o :: Ordering) (lt :: k) (eq :: k) (gt :: k) :: k where
  CaseOrdering 'LT lt eq gt = lt
  CaseOrdering 'EQ lt eq gt = eq
  CaseOrdering 'GT lt eq gt = gt

-- | Given two 'Nat's @m@ and @n@, computes @m - n@ as an 'Integer'.
type family (m :: Nat) -# (n :: Nat) where
  -- Redundant cases solely to make sure we get stuck reducing '-#' rather
  -- than reducing @CaseOrdering (Cmp m n) tons of junk@ when the magnitude is
  -- a type variable.  This is similar to adding a bang pattern or case
  -- statement to a term-level function to make it strict in its arguments; but
  -- we don't have case statements or bang patterns in type families, so we add
  -- clauses instead.
  n -# 0 = 'Pos n  -- Order matters: 0 -# 0 should be 'Pos 0
  0 -# n = 'Neg n

  m -# n = CaseOrdering (Cmp m n)
    ('Neg (n - m))
    ('Pos 0)
    ('Pos (m - n))

-- | Addition of type-level integers.
type family AddInteger m n where
  AddInteger ('Pos m) ('Pos n) = 'Pos (m + n)
  AddInteger ('Pos m) ('Neg n) = m -# n
  AddInteger ('Neg m) ('Pos n) = n -# m
  AddInteger ('Neg m) ('Neg n) = 'Neg (m + n)

-- | Subtraction of type-level integers.
type family SubInteger m n where
  -- Note we could define this as @AddInteger m (Negate n)@, but then GHC would
  -- reduce eagerly to this when the parameters are type variables, and
  -- irreducible calls to 'SubInteger' would appear in error messages as
  -- @AddInteger m (Negate n)@ rather than the more readable @SubInteger m n@.
  -- So, we tolerate some duplicated code in order to make sure we get stuck on
  -- 'SubInteger' rather than later.
  SubInteger ('Pos m) ('Pos n) = m -# n
  SubInteger ('Pos m) ('Neg n) = 'Pos (m + n)
  SubInteger ('Neg m) ('Pos n) = 'Neg (m + n)
  SubInteger ('Neg m) ('Neg n) = n -# m
