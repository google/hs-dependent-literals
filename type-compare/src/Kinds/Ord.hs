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

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Type-level 'Ord'.
--
-- On recent versions of @base@, this just re-exports things from
-- @Data.Type.Ord@.  On older versions, it provides its own implementation,
-- moved from older versions of @numeric-kinds@.

module Kinds.Ord
         ( -- * Comparisons
           Compare
         , type (<?), type (<=?), type (==?), type (/=?), type (>=?), type (>?)

           -- ** Inequality Constraints
         , type (<), type (<=), type (==), type (/=), type (>=), type (>)

           -- ** Selection
         , Max, Min

           -- * Utility
         , Proven, OrdCond, CompareCond
         ) where

#if MIN_VERSION_base(4, 16, 0)
import Data.Type.Ord
         ( Compare, OrdCond
         , type (<?), type (>?), type (<=?), type (>=?)
         , type (<=), type (>=), type (>)
         , Max, Min
         )
#else
import GHC.TypeLits (CmpNat)

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

-- | @CompareCond x y lt eq gt@ is @lt@ if @x@ is less than @y@, and so on.
type CompareCond x y lt eq gt = OrdCond (Compare x y) lt eq gt

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

type x <?  y = CompareCond x y True False False
type x >?  y = CompareCond x y False False True
type x <=? y = CompareCond x y True True False
type x >=? y = CompareCond x y False True True
#endif

infix ==?, /=?

type x ==? y = CompareCond x y False True False
type x /=? y = CompareCond x y True False True

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

#if !MIN_VERSION_base(4, 16, 0)
type Min x y = CompareCond x y x x y
type Max x y = CompareCond x y y y x
#endif
