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

module IntErrors where

import Data.Int (Int8, Int16)

x0, x1 :: Int8
-- Want: 128 is too large
x0 = 128
-- Want: -129 is too small
x1 = -129

x2 :: Int8
-- Want: out of range for Int, for conversion to Int8
x2 = 9223372036854775808

x3, x4 :: Int16
-- Want: 32768 is too large
x3 = 32768
-- Want: -32769 is too small
x4 = -32769

x5, x6 :: Int
-- Want: too large
x5 = 9223372036854775808
-- Want: too small
x6 = -9223372036854775809
