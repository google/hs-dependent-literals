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

{-# LANGUAGE ScopedTypeVariables #-}

module SIntPatterns where

import Data.Fin.Int (Fin)
import Data.SInt (SInt)
import GHC.TypeNats (KnownNat)

import TestUtils (showTyped)

-- Easy: just match on the right value.
x0 :: SInt 2 -> String
x0 x = case x of
  2 -> "2"
  _ -> "inconceivable"

-- We can match on multiple values for unknown SNats.
x1 :: SInt n -> String
x1 x = case x of
  0 -> "0"
  1 -> "1"
  _ -> "other"

-- When matching literals, we get an equality proof.  Inside each arm we know
-- @n@ statically and GHC will natively solve KnownNat constraints for it.
x2 :: forall n. SInt n -> String
x2 x = case x of
  0 -> "uninhabited"
  1 -> f 0
  4 -> f 2
  16 -> f 7
  _ -> "whatever"
 where
  f :: KnownNat n => Fin n -> String
  f = showTyped
