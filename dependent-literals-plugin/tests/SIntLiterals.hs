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

module SIntLiterals where

import Data.SInt (SInt)

import TestUtils (theSInt, asFin, showTyped)

-- Easy: can we use a literal given that we know it matches the type?
x0 :: SInt 24
x0 = 24

-- Can we infer the Nat index from the literal?
x1 :: String
x1 = showTyped (theSInt 24)

-- Can we use it to pin down the Nat index of a Fin?
x2 :: String
x2 = showTyped (10 `asFin` 24)
