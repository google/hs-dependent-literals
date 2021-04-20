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

{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE UndecidableInstances #-}

module WithNegativeLiterals where

import Data.Int (Int8, Int16, Int32, Int64)

x0 :: Int8
x0 = -128

x1 :: Int16
x1 = -32768

x2 :: Int32
x2 = -2147483648

x3 :: Int64
x3 = -9223372036854775808
