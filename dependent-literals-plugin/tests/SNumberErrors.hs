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

module SNumberErrors where

import Data.SNumber (SNumber)
import Kinds.Integer (Integer(..))

-- Want: could not match type 3 with 2
x0 :: SNumber Int ('Pos 2)
x0 = 3

-- Want: some form of "out of range"
x1 :: SNumber Int ('Pos 9223372036854775808)
x1 = 9223372036854775808

-- Want: could not match type 3 with 2
x2 :: SNumber Word ('Pos 2)
x2 = 3

x3 :: SNumber Word ('Neg 2)
x3 = -2

x4 :: SNumber Int ('Pos 2) -> String
x4 x = case x of
  0 -> "want inaccessible code error: 0 /= 2"
  -1 -> "want inaccessible code error: -1 is negative"
  _ -> "_"

-- Want: could not match
x5 :: SNumber Int n
x5 = 4

-- Want: does not match type index.
x6 :: SNumber Int ('Pos 0)
x6 = -0

-- Want: does not match type index.
x7 :: SNumber Int n
x7 = -0

-- Want: illegal -0
x8 :: SNumber Int ('Neg 0)
x8 = -0

-- Want: don't know enough about a.
x9 :: SNumber a ('Pos 0)
x9 = 0
