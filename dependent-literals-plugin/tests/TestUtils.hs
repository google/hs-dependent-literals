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

module TestUtils
         ( type (<), theFin, theSInt
         , atNat, asFin, showTyped, showsPrecTyped
         ) where

import Data.Fin.Int (Fin)
import Data.SInt (SInt)
import Data.Typeable (Typeable, typeOf)
import GHC.TypeNats (CmpNat)

type m < n = CmpNat m n ~ 'LT

theSInt :: SInt n -> SInt n
theSInt = id

theFin :: Fin n -> Fin n
theFin = id

atNat :: SInt n -> p n -> p n
atNat _ = id

asFin :: Fin n -> SInt n -> Fin n
asFin = const

showsPrecTyped :: (Show a, Typeable a) => Int -> a -> ShowS
showsPrecTyped p x = showParen (p > 0) $
  shows x . showString " :: " . shows (typeOf x)

showTyped :: (Show a, Typeable a) => a -> String
showTyped x = showsPrecTyped 0 x ""
