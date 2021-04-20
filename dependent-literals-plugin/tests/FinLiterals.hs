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

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module FinLiterals where

import DependentLiterals (AllowsIntLiteral)
import GHC.TypeNats (CmpNat)
import Kinds.Integer (pattern Pos)
import Data.Fin.Int (Fin)

type m < n = CmpNat m n ~ 'LT

x0 :: Fin 1
x0 = 0

x1 :: Fin 1024
x1 = 42

x2 :: (4 < n) => Fin n
x2 = 4

x3 :: AllowsIntLiteral 4 (Fin n) => Fin n
x3 = 4

x4 :: AllowsIntLiteral ('Pos 4) (Fin n) => Fin n
x4 = 4
