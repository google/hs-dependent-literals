-- Copyright 2021 Google LLC
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
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TupleIndexing where

import Prelude hiding ((!!))

import GHC.TypeNats (type (-), Nat)
import Data.Kind (Type)
import Data.SNumber (SNumber(N#))
import Kinds.Integer qualified as K
import Kinds.Integer (pattern Pos)
import DependentLiterals.Int
         ( SNum(..), Satisfying(..), HasIntLiterals(..), SNumLit(..)
         )
import Numeric.Natural (Natural)
import Unsafe.Coerce (UnsafeEquality(..), unsafeEqualityProof)

data Peano = Z | S Peano

data SPeano (n :: Peano) where
  SZ :: SPeano 'Z
  SS :: SPeano n -> SPeano ('S n)

type family ToPeano (n :: Nat) :: Peano where
  ToPeano 0 = 'Z
  ToPeano n = 'S (ToPeano (n - 1))

type family IntegerToNat (n :: K.Integer) :: Nat where
 IntegerToNat ('Pos x) = x

class (n ~ ToPeano (IntegerToNat m)) => SPeanoConstraint n m
instance (n ~ ToPeano (IntegerToNat m)) => SPeanoConstraint n m

data Some (f :: k -> Type) = forall a. Some (f a)

instance SNum (SPeano n) where
  type SNumRepr (SPeano n) = Natural
  type SNumConstraint (SPeano n) = SPeanoConstraint n
  fromSNum (Satisfying (N# x0)) =
    case go x0 of
      Some (sp :: SPeano m) ->
        case unsafeEqualityProof @m @n of UnsafeRefl -> sp
   where
    go :: Natural -> Some SPeano
    go 0 = Some SZ
    go n = case go (n-1) of Some sp -> Some (SS sp)
  intoSNum x = case unsafeEqualityProof @n @(ToPeano (IntegerToNat ('Pos 0))) of
    UnsafeRefl -> Satisfying (N# @('Pos 0) (go x))
   where
    go :: forall m. SPeano m -> Natural
    go SZ = 0
    go (SS m) = 1 + go m

class SPeanoConstraint n m => SPeanoAssertion n t m
instance SPeanoConstraint n m => SPeanoAssertion n t m

deriving via SNumLit (SPeanoAssertion n) (SPeano n)
  instance HasIntLiterals (SPeano n)

data HList (ts :: [Type]) where
  HNil :: HList '[]
  HCons :: a -> HList ts -> HList (a ': ts)

type family Index (ts :: [a]) (m :: Peano) :: a where
  Index (x ': xs) 'Z = x
  Index (x ': xs) ('S n) = Index xs n

index :: HList ts -> SPeano i -> Index ts i
index (HCons x _) SZ = x
index  (HCons _ xs) (SS i) = index xs i
index _ _ = error "impossible"

class Tuple a ts | a -> ts, ts -> a where
  toHList :: a -> HList ts
  fromHList :: HList ts -> a

instance Tuple () '[] where
  toHList () = HNil
  fromHList HNil = ()

instance Tuple (x, y) '[x, y] where
  toHList (x, y) = HCons x (HCons y HNil)
  fromHList (HCons x (HCons y HNil)) = (x, y)

instance Tuple (x, y, z) '[x, y, z] where
  toHList (x, y, z) = HCons x (HCons y (HCons z HNil))
  fromHList (HCons x (HCons y (HCons z HNil))) = (x, y, z)

(!!) :: Tuple a ts => a -> SPeano i -> Index ts i
x !! i = index (toHList x) i

example :: Int
example = ("aoeu", 42) !! 1

example1 :: String
example1 = (0 :: Int, False, "hi") !! 2
