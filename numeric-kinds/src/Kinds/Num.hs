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

-- | Type-level 'Ord' and 'Num'.
--
-- This provides "kindclasses" (actually open type families) with functionality
-- analogous to that provided by the typeclasses 'Ord' and 'Num'.  Since
-- type-level typeclasses don't exist, instead we translate each would-be
-- method to its own open type family; then "instances" are implemented by
-- providing clauses for each type family "method".  Unfortunately this means
-- we can't group methods into classes that must be implemented all-or-none,
-- but in practice this seems to be okay.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Kinds.Num
         ( -- * Comparisons
           Cmp
         , type (<?), type (<=?), type (==?), type (/=?), type (>=?), type (>?)

           -- ** Inequality Constraints
         , type (<), type (<=), type (==), type (/=), type (>=), type (>)

           -- * Conversions
         , FromNat, ToInteger

           -- * Arithmetic
         , type (+), type (-), type (*)

           -- * Utility
         , Proven, IsLT, IsLE, IsGT, IsGE, IsEQ, IsNE
         ) where

import Prelude hiding (Integer)

import GHC.TypeNats (CmpNat, Nat)
import qualified GHC.TypeNats as N (type (+), type (-), type (*))

import {-# source #-}  Kinds.Integer (Integer(..))

-- | Type-level Ord "kindclass".
--
-- Note this has an invisible dependent @k@ parameter that makes the
-- textually-identical instances for different kinds actually different.  Neat!
type family Cmp (x :: k) (y :: k) :: Ordering

-- That is, this is @Cmp {k=Nat} x y@, which is distinct from
-- @Cmp {k=Integer} x y@, even though both of them are written as @Cmp x y@.
type instance Cmp {- k=Nat -} x y = CmpNat x y

-- | Type-level numeric conversion from 'Nat'.  Like 'fromInteger' in 'Num'.
type family FromNat (n :: Nat) :: k

type instance FromNat {- k=Nat -} n = n

-- | Type-level conversion to 'Integer'.  Like 'toInteger' in 'Integral'.
type family ToInteger (n :: k) :: Integer

type instance ToInteger {- k=Nat -} n = 'Pos n

-- | Type-level addition "kindclass".
type family (x :: k) + (y :: k) :: k

type instance x + y = (N.+) x y  -- HLint doesn't like qualified TypeOperators.

-- | Type-level subtraction "kindclass".
type family (x :: k) - (y :: k) :: k

type instance x - y = (N.-) x y

-- | Type-level multiplication "kindclass".
type family (x :: k) * (y :: k) :: k

type instance x * y = (N.*) x y

infix 4 <?, >?, <=?, >=?, ==?, /=?

type x <?  y = IsLT (Cmp x y)
type x >?  y = IsGT (Cmp x y)
type x <=? y = IsLE (Cmp x y)
type x >=? y = IsGE (Cmp x y)
type x ==? y = IsEQ (Cmp x y)
type x /=? y = IsNE (Cmp x y)

-- | Turns a type-level 'Bool' into a 'Data.Kind.Constraint' that it's 'True'.
type Proven b = b ~ 'True

infix 4 <, >, <=, >=, ==, /=

type x <  y = Cmp x y ~ 'LT
type x >  y = Cmp x y ~ 'GT
type x == y = Cmp x y ~ 'EQ
type x <= y = Proven (x <=? y)
type x >= y = Proven (x >=? y)
type x /= y = Proven (x /=? y)

-- | Test whether an 'Ordering' is 'LT'.
type family IsLT o where
  IsLT 'LT = 'True
  IsLT o   = 'False

-- | Test whether an 'Ordering' is 'LT' or 'EQ'.
type family IsLE o where
  IsLE 'GT = 'False
  IsLE o   = 'True

-- | Test whether an 'Ordering' is 'GT'.
type family IsGT o where
  IsGT 'GT = 'True
  IsGT o   = 'False

-- | Test whether an 'Ordering' is 'GT' or 'EQ'.
type family IsGE o where
  IsGE 'LT = 'False
  IsGE o   = 'True

-- | Test whether an 'Ordering' is 'EQ'.
type family IsEQ o where
  IsEQ 'EQ = 'True
  IsEQ o   = 'False

-- | Test whether an 'Ordering' is 'LT' or 'GT'.
type family IsNE o where
  IsNE 'EQ = 'False
  IsNE o   = 'True
