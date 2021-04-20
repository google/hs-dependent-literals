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

module FinPatterns where

import Data.Fin.Int (Fin)

-- Easy: we know @n@ statically, and don't need the provided constraints.
simpleMatch :: Fin 4 -> String
simpleMatch x = case x of
  0 -> "0"
  1 -> "1"
  _ -> "2 or 3"

-- Harder: we have to compare against 0/1 without knowing it's in range.
polyMatch :: Fin n -> String
polyMatch x = case x of
  0 -> "0"
  1 -> "1"
  _ -> "unknown"

-- Even harder: comparing against 2 has to prove that 2 is in range.
usingEvidence :: Fin n -> Maybe (Fin n)
usingEvidence x = case x of
  2 -> Just 2
  _ -> Nothing
