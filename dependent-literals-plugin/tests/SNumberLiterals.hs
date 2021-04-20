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

{-# LANGUAGE TypeApplications #-}

module SNumberLiterals where

import Data.SNumber (SNumber)
import DependentLiterals (valueOf)
import Kinds.Integer (Integer(..))
import Numeric.Natural (Natural)

x0 :: SNumber Natural ('Pos 0)
x0 = 0

x1 :: SNumber Natural ('Pos 4096)
x1 = 4096

x2 :: SNumber Natural ('Pos 77)
x2 = valueOf @77

x3 :: SNumber Natural ('Pos 77)
x3 = valueOf @('Pos 77)

x4 :: SNumber Int ('Pos 0)
x4 = 0

x5 :: SNumber Int ('Neg 4)
x5 = -4

x6 :: SNumber Int ('Neg 77)
x6 = valueOf @('Neg 77)

x7 :: SNumber Word ('Pos 7)
x7 = 7

x8 :: SNumber Prelude.Integer ('Neg 44)
x8 = -44
