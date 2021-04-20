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

{-# OPTIONS -Wwarn=inaccessible-code #-}

module FinErrors where

import Data.Fin.Int (Fin)

x0 :: Fin 0
-- Want: out of range.
x0 = 0

x1, x2 :: Fin 2
-- Want: out of range.
x1 = 2
-- Want: out of range.
x2 = -2

x3 :: Fin 2 -> String
x3 x = case x of
  0 -> "hi"
  3 -> "want unreachable code" -- TODO these could have better error messages
  -1 -> "want unreachable code"
  _ -> "other"

-- Want: not sure it's in range
x4 :: Fin n
x4 = 42

-- Want: illegal -0
x5 :: Fin 1
x5 = -0

-- Since this literal overflows (LitRepr (Fin n) ~ Int), it's liable to
-- generate proof 18446744073709551618 < n when given any (2 :: Fin n), and
-- then make an ill-formed Fin of that value.  We have constraints on the
-- underlying LitRepr precisely to prevent this: because this literal cannot be
-- faithfully represented by LitRepr (Fin n), even if the instance for Fin n
-- thinks it can accept the literal, we must reject it.
x100 :: Fin n -> Fin n
x100 x = case x of
  18446744073709551618 -> 18446744073709551618
  _ -> x

-- This also needs to be a type error: if it matched (which it would for 2, if
-- we generated code for it), we would be able to create a proof that
-- 18446744073709551618 < n for any n > 2.
x101 :: Fin n -> Fin n
x101 x = case x of
  18446744073709551618 -> x
  _ -> x
