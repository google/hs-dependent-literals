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

module WordPatterns where

import Data.Word (Word8, Word16, Word32, Word64)

f :: Word -> String
f x = case x of
  1 -> "1"
  2 -> "2"
  _ -> "_"

f8 :: Word8 -> String
f8 x = case x of
  1 -> "1"
  2 -> "2"
  _ -> "_"

f16 :: Word16 -> String
f16 x = case x of
  1 -> "1"
  2 -> "2"
  _ -> "_"

f32 :: Word32 -> String
f32 x = case x of
  1 -> "1"
  2 -> "2"
  _ -> "_"

f64 :: Word64 -> String
f64 x = case x of
  1 -> "1"
  2 -> "2"
  _ -> "_"
