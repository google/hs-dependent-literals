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

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RoleAnnotations #-}
#if !defined(__HLINT__)
{-# LANGUAGE StandaloneDeriving #-}
#endif
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module DependentLiterals.Bounds
         ( -- * Bounds Checks

           -- ** Error Messages
           OutOfRangeMsg

           -- ** Error Message Utilities
         , ShowTypedNum, ShowRange

           -- ** Error Constraints
         , OutOfRangeErr

           -- ** Inequality Assertions
         , CheckLessThanMaxBound, CheckAtLeastMinBound, AssertEq, AssertNotApart

           -- * Implementation Details
         , ShowNum, AssertNotApart_, Eql, FailedToProveEq
         ) where

import Data.Kind (Constraint, Type)
import GHC.TypeLits (TypeError, ErrorMessage(..))

import Kinds.Integer (pattern Pos, pattern Neg)
import Kinds.Num (type (-))
import Kinds.Ord (type (>=?), type (<?), type (<), type (>=))
import qualified Kinds.Integer as K (Integer)

type family ShowNum (n :: K.Integer) where
  ShowNum ('Pos n) = 'ShowType n
  ShowNum ('Neg n) = 'Text "-" ':<>: 'ShowType n

type ShowTypedNum a n = ShowNum n ':<>: 'Text " :: " ':<>: 'ShowType a

type ShowRange min maxp1 =
  'Text "(" ':<>: ShowNum min ':<>: 'Text ".." ':<>:
  ShowNum (maxp1 - 'Pos 1) ':<>: 'Text ")"

type OutOfRangeMsg min maxp1 a n =
  'Text "Literal out of range " ':<>: ShowRange min maxp1 ':<>: 'Text ":" ':$$:
  'Text "  " ':<>: ShowTypedNum a n

class OutOfRangeErr (min :: K.Integer) (maxp1 :: K.Integer) (a :: Type) (n :: K.Integer)
instance TypeError (OutOfRangeMsg min maxp1 a n) => OutOfRangeErr min maxp1 a n

type family Eql a b :: Bool where
  Eql a a = 'True
  Eql a b = 'False

class a ~ b => AssertEq (c :: Constraint) a b
instance AssertEq c a a

-- | If you tried to prove a constraint and failed, and want to issue a custom
-- error message for it explicitly, write something like this.
--
-- Given "class _c => FailedToProveC (err :: Constraint) ...",
-- "FailedToProveC (TypeError ...)" is a constraint that pretends to prove @c@
-- but instead throws a type error.
class a ~ b => FailedToProveEq (err :: Constraint) a b

class a ~ b => AssertNotApart_ (msg :: ErrorMessage) eq a b
instance a ~ b => AssertNotApart_ msg 'True a b
instance FailedToProveEq (TypeError msg) a b => AssertNotApart_ msg 'False a b

type AssertNotApart msg a b = AssertNotApart_ msg (Eql a b) a b

class (n < maxp1)
   => CheckLessThanMaxBound
        (msg :: ErrorMessage)
        (maxp1 :: K.Integer)
        (a :: Type)
        (n :: K.Integer)
instance AssertNotApart msg (n <? maxp1) 'True
      => CheckLessThanMaxBound msg maxp1 a n

class (n >= min)
   => CheckAtLeastMinBound
        (msg :: ErrorMessage)
        (min :: K.Integer)
        (a :: Type)
        (n :: K.Integer)
instance AssertNotApart msg (n >=? min) 'True
      => CheckAtLeastMinBound msg min a n
