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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.SNumber.Internal
  ( NegativeReprUnsignedErr
  , IsAtLeastMinBound, IsLessThanMaxBound
  , OutOfReprRangeErr, ForbidNegZero
  ) where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (TypeError, ErrorMessage(..))

import Prelude hiding (Integer) -- No term-level stuff in this module anyway.
import Kinds.Integer (Integer(..))
import Kinds.Num (type (-))
import Kinds.Ord (type (<?), type (>=?), Compare)

type family ShowNum (n :: Integer) where
  ShowNum ('Pos n) = 'ShowType n
  ShowNum ('Neg n) = 'Text "-" ':<>: 'ShowType n

type ShowTypedNum a n = ShowNum n ':<>: 'Text " :: " ':<>: 'ShowType a

type ShowRange min maxp1 =
  'Text "(" ':<>: ShowNum min ':<>: 'Text ".." ':<>:
  ShowNum (maxp1 - 'Pos 1) ':<>: 'Text ")"

type NegativeReprUnsignedMsg repr a n =
  'Text "Negative SNumber with unsigned witness type:" ':$$:
  'Text "  " ':<>: ShowTypedNum (a n) n

class NegativeReprUnsignedErr repr (a :: Integer -> Type) (n :: Integer)
instance TypeError (NegativeReprUnsignedMsg repr a n)
      => NegativeReprUnsignedErr repr a n

type OutOfReprRangeMsg min maxp1 repr a n =
  'Text "SNumber overflows witness type:" ':$$:
  'Text "  " ':<>: ShowTypedNum (a n) n ':$$:
  'Text "The representable range is " ':<>: ShowRange min maxp1

class OutOfReprRangeErr
        (min :: Integer)
        (maxp1 :: Integer)
        repr
        (a :: Integer -> Type)
        (n :: Integer)
instance TypeError (OutOfReprRangeMsg min maxp1 repr a n)
      => OutOfReprRangeErr min maxp1 repr a n

type family Assert b msg :: Constraint where
  Assert 'True  c = ()
  Assert 'False c = c

type family AssertLessThanMaxBound
    u
    (n :: Integer)
    (maxp1 :: Integer)
    (err :: Constraint) where
  AssertLessThanMaxBound '() n maxp1 err = Assert (n <? maxp1) err

type family Reduce (x :: k) :: ()
type instance Reduce 'LT = '()
type instance Reduce 'EQ = '()
type instance Reduce 'GT = '()
type instance Reduce 'True = '()
type instance Reduce 'False = '()

class IsLessThanMaxBound
        (maxp1 :: Integer)
        (err :: Integer -> Constraint)
        (n :: Integer)
instance AssertLessThanMaxBound (Reduce (Compare n maxp1)) n maxp1 (err n)
      => IsLessThanMaxBound maxp1 err n

-- | Assert that a numeric literal is greater than the (negative) min bound.
--
-- This class name is semi-user-facing: it can appear in error messages when
-- trying to use literals of polymorphic or ambiguous types.
type family AssertAtLeastMinBound u (n :: Integer) (min :: Integer) err where
   AssertAtLeastMinBound '() n min err = Assert (n >=? min) err

class IsAtLeastMinBound
        (min :: Integer)
        (err :: Integer -> Constraint)
        (n :: Integer)
instance AssertAtLeastMinBound (Reduce (Compare n min)) n min (err n)
      => IsAtLeastMinBound min err n

type family ErrorIfNegZero n :: Constraint where
  ErrorIfNegZero ('Neg 0) = TypeError ('Text "Illegal SNumber -0")
  ErrorIfNegZero x = ()

class ForbidNegZero (n :: Integer)
instance ErrorIfNegZero n => ForbidNegZero n
