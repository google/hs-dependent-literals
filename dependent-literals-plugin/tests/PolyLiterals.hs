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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module PolyLiterals where

import DependentLiterals (IntLiteral, HasBasicLiterals)
import Data.Vec.Short (Vec, (!))
import qualified Data.Vec.Short as V
import GHC.TypeNats (CmpNat)
import Data.Fin.Int (Fin, (+!))

-- Will be exported from Fin, later.
type m < n = CmpNat m n ~ 'LT

xx0 :: Int
xx0 = f (V.fromList [0, 2, 3, 7] :: Vec 4 Int)
 where
  f v = v ! 2

xx1 :: Fin 4
xx1 = f (0 :: Fin 4)
 where
  f x = 1 +! x

x0 :: IntLiteral 4 a => a
x0 = 4

x1 :: HasBasicLiterals a => a
x1 = 4

x2 :: HasBasicLiterals a => a
x2 = x0

x3 :: Fin 4
x3 = x
 where
  x = 3

theFin :: Fin n -> Fin n
theFin = id

x4 :: Fin 4
x4 = x
 where
  x = theFin 3

x5 :: Fin 4
x5 = x
 where
  x :: IntLiteral 3 a => a
  x = 3

x6 :: (3 < n) => Fin n
x6 = x
 where
  x = 3

x7 :: (3 < n) => Fin n
x7 = x
 where
  x = theFin 3
