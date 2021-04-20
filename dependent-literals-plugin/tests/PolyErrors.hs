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

-- Provoking error messages with various amounts of context known about the
-- HasIntLiterals machinery.

module PolyErrors where

import DependentLiterals (HasIntLiterals)
import Data.Fin.Int (Fin)
import Data.SInt (SInt)

-- With absolutely no context.
x1, x2 :: a
-- Want: no instance HasIntLiterals
-- TODO: this generates two errors currently.
x1 = 0
-- Want: no instance HasIntLiterals
-- TODO: this generates two errors currently.
x2 = -1

-- With context that @a@ has an instance, but nothing else.
x3, x4 :: HasIntLiterals a => a
-- Want: something to the effect that we don't know enough about a?
-- TODO: this generates two errors currently.
x3 = 0
-- Want: something to the effect that we don't know enough about a?
-- TODO: not very good errors
x4 = -1

-- With context that @a@ is some @Fin n@.
x5, x6 :: Fin n
-- Want: could not deduce IsLessThanMaxBound
x5 = 0
-- Want: negative literal for unsigned type.
-- TODO: this generates two errors currently.
x6 = -1

-- With context that @a@ is some @SInt n@.
x7, x8 :: SInt n
-- Want: could not match type n with 0
x7 = 0
-- Want: negative literal for unsigned type
-- TODO: this generates 3 errors currently.
x8 = -1
