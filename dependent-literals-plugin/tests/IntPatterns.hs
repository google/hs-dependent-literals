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

module IntPatterns where

import Data.Int (Int8, Int16, Int32, Int64)

x0 :: Int -> String
x0 x = case x of
  1 -> "1"
  -2 -> "2"
  _ -> "_"

x1 :: Int8 -> String
x1 x = case x of
  1 -> "1"
  -2 -> "2"
  _ -> "_"

x2 :: Int16 -> String
x2 x = case x of
  1 -> "1"
  -2 -> "2"
  _ -> "_"

x3 :: Int32 -> String
x3 x = case x of
  1 -> "1"
  -2 -> "2"
  _ -> "_"

x4 :: Int64 -> String
x4 x = case x of
  1 -> "1"
  -2 -> "2"
  _ -> "_"
