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
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeOperators #-}

module DoubleNegation where

-- HLint doesn't parse the thing we're trying to test here, so shut it up.
#if !defined(__HLINT__)
import Data.Fin.Int (Fin)
import Data.Word (Word8)
import Data.Type.Equality ((:~:)(Refl))
import Data.SInt (SInt)

-- Turns out you can have both syntactic negation and a negative literal in the
-- same pattern/expression.  We should not have a bug when doing that.

x0 :: Int -> String
x0 x = case x of
  -1 -> "-1"
  - -2 -> "2...?"
  _ -> "_"

-- Make sure we in fact get proof for +2 out of matching on @- -2@.
x1 :: SInt n -> Maybe (n :~: 2)
x1 x = case x of
  - -2 -> Just Refl
  _ -> Nothing

x2 :: Word8
x2 = - -8

x3 :: Fin 5
x3 = - -4

x4 :: SInt 2 -> SInt 2
x4 x = case x of
  - -2 -> x
  _ -> x
#endif
