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

module WordErrors where

import Data.Word (Word8, Word16, Word)

-- Want -Woverflowed-literals warnings for all of these.

x1, x2, x3 :: Word8
x1 = -1
x2 = -256
x3 = 256

x4, x5, x6 :: Word16
x4 = -1
x5 = -65536
x6 = 65536

x7, x8, x9 :: Word
x7 = -1
x8 = -18446744073709551616
x9 = 18446744073709551616
