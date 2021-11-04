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

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Kinds.Num
         ( -- * Comparisons
           Cmp, Compare
         , type (<?), type (<=?), type (==?), type (/=?), type (>=?), type (>?)

           -- ** Inequality Constraints
         , type (<), type (<=), type (==), type (/=), type (>=), type (>)

           -- * Conversions
         , FromNat, ToInteger

           -- * Arithmetic
         , type (+), type (-), type (*)

           -- * Utility
         , Proven, OrdCond
         ) where

import Prelude hiding (Integer)

import GHC.TypeNats (Nat)
import qualified GHC.TypeNats as N (type (+), type (-), type (*))

import {-# source #-}  Kinds.Integer (Integer(..))

#if MIN_VERSION_base(4, 16, 0)
import Data.Type.Ord
         ( Compare, OrdCond
         , type (<?), type (>?), type (<=?), type (>=?)
         , type (<=), type (>=), type (>)
         )
#else
import GHC.TypeNats (CmpNat)

-- | Type-level Ord "kindclass".
--
-- Note this has an invisible dependent @k@ parameter that makes the
-- textually-identical instances for different kinds actually different.  Neat!
type family Compare (x :: k) (y :: k) :: Ordering

-- That is, this is @Compare {k=Nat} x y@, which is distinct from
-- @Compare {k=Integer} x y@, even though both of them are written as
-- @Compare x y@.
type instance Compare {- k=Nat -} x y = CmpNat x y
#endif

-- | Backwards-compatibility alias for 'Compare'.
--
-- Prefer 'Compare' over this.
type Cmp x y = Compare x y

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

-- Recently added to base.
#if !MIN_VERSION_base(4, 16, 0)

-- | Type-level eliminator for 'Ordering'.
--
-- @OrdCond o lt eq gt@ selects from among @lt@, @eq@, and @gt@ according
-- to @o@.
type family OrdCond (o :: Ordering) (lt :: k) (eq :: k) (gt :: k) :: k where
  OrdCond 'LT lt eq gt = lt
  OrdCond 'EQ lt eq gt = eq
  OrdCond 'GT lt eq gt = gt

infix 4 <?, >?, <=?, >=?

type x <?  y = OrdCond (Compare x y) True False False
type x >?  y = OrdCond (Compare x y) False False True
type x <=? y = OrdCond (Compare x y) True True False
type x >=? y = OrdCond (Compare x y) False True True
#endif

infix ==?, /=?

type x ==? y = OrdCond (Compare x y) False True False
type x /=? y = OrdCond (Compare x y) True False True

-- | Turns a type-level 'Bool' into a 'Data.Kind.Constraint' that it's 'True'.
type Proven b = b ~ 'True

-- Recently added to base.
#if !MIN_VERSION_base(4, 16, 0)
infix 4 <=, >=, >

type x <= y = Proven (x <=? y)
type x >= y = Proven (x >=? y)
type x >  y = Proven (x >? y)
#endif

infix 4 <, ==, /=

-- The base-4.16.0.0 version of (<) is wrong.
type x <  y = Proven (x <? y)
type x == y = Proven (x ==? y)
type x /= y = Proven (x /=? y)
