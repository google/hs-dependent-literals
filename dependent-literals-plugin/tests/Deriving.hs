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
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Deriving where

import Data.Int (Int8)
import Data.SNumber (SNumber)
import DependentLiterals (HasIntLiterals, StockLit(..))
import GHC.TypeNats (type (*))
import Kinds.Integer (Integer(..))
import Data.Fin.Int (Fin)

newtype MyType = MyType Int
#if !defined(__HLINT__)
  deriving HasIntLiterals via StockLit Int
#endif

x0 :: MyType
x0 = 0

-- TODO(awpr): it'd be nice to have this trigger -Woverflowed-literals;
-- currently the plugin passes this number as @Num MyType => MyType@, which
-- doesn't trigger it.  Sadly I think if we tried to use type family machinery
-- to pass it as Int, it still wouldn't trigger, because the warning checks
-- whether the type /is/ the unique identifier Int, not whether it evaluates to
-- Int.
x1 :: MyType
x1 = 9223372036854775808

newtype MyType2 = MyType2 Int
  deriving HasIntLiterals

x2 :: MyType2
x2 = 0

newtype MyType3 = MyType3 (Fin 4)
  deriving HasIntLiterals

x3 :: MyType3
x3 = 2

newtype MyType4 n = MyType4 (SNumber Int ('Pos (2 * n)))
  deriving HasIntLiterals

x4 :: MyType4 77
x4 = 154

newtype MyInt8 = MyInt8 Int8
  deriving HasIntLiterals

x5 :: MyInt8
x5 = 42

x6 :: MyInt8
x6 = -7
