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

-- | Type-level equivalent of a subset of 'Num'.
--
-- This provides "kindclasses" (actually open type families) with functionality
-- analogous to that provided by the 'Num' typeclass.  Since type-level
-- typeclasses don't exist, instead we translate each would-be method to its
-- own open type family; then "instances" are implemented by providing clauses
-- for each type family "method".  Unfortunately this means we can't group
-- methods into classes that must be implemented all-or-none, but in practice
-- this seems to be okay.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Kinds.Num
         ( -- * Conversions
           FromNat, ToInteger

           -- * Arithmetic
         , type (+), type (-), type (*)
         ) where

import Prelude hiding (Integer)

import GHC.TypeNats (Nat)
import qualified GHC.TypeNats as N (type (+), type (-), type (*))

import {-# source #-}  Kinds.Integer (Integer(..))

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
