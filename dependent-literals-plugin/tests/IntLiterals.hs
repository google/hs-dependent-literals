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

module IntLiterals where

import Data.Int (Int8, Int16, Int32, Int64)

x :: Int
x = 4

-- We should be able to use -0 just fine for types with basic literals.
x1 :: Int
x1 = -0

x8 :: Int8
x8 = 4

x16 :: Int16
x16 = 4

x32 :: Int32
x32 = 4

x64 :: Int64
x64 = 4

y :: Int
y = -4

y8 :: Int8
y8 = -4

y16 :: Int16
y16 = -4

y32 :: Int32
y32 = -4

y64 :: Int64
y64 = -4
