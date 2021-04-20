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

module FractionalPatterns where

x0 :: Float -> String
x0 x = case x of
  0 -> "0"
  -2 -> "-2"
  2048 -> "2048"
  0.1 -> "0.1" -- Make sure we don't interfere with fractional literals... yet.
  _ -> "_"

x1 :: Double -> String
x1 x = case x of
  0 -> "0"
  -2 -> "-2"
  2048 -> "2048"
  0.1 -> "0.1" -- Make sure we don't interfere with fractional literals... yet.
  _ -> "_"

x2 :: Rational -> String
x2 x = case x of
  0 -> "0"
  -2 -> "-2"
  2048 -> "2048"
  0.1 -> "0.1"
  _ -> "_"
