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

module SIntErrors where

import Data.SInt (SInt)

-- Want: could not match type 3 with 2
x0 :: SInt 2
x0 = 3

-- Want: negative literal for unsigned type
-- TODO: this generates 3 errors currently.
x1 :: SInt 0
x1 = -1

x2 :: SInt 2 -> String
x2 x = case x of
  0 -> "want inaccessible code error: 0 /= 2"
  -1 -> "want inaccessible code error: -1 is negative"
  _ -> "_"

-- Want: could not match
x3 :: SInt n
x3 = 4

-- Want: does not match type index.
x4 :: SInt 0
x4 = -0

-- Want: does not match type index.
x5 :: SInt n
x5 = -0
