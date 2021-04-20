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

-- | Top-level module exporting the key DependentLiterals functionality.

module DependentLiterals
  ( -- * Int Literals
    HasIntLiterals, AllowsIntLiteral, IntLiteral
  , valueOf

    -- ** Basic Literals
  , HasBasicLiterals, StockLit(..)

    -- ** Dependent Literals
  , SNum(..), SNumLit(..)
  ) where

import DependentLiterals.Int
