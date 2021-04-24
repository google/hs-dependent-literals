-- Copyright 2021 Google LLC
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

module VecExamples where

import Data.Vec.Short.Explicit

-- Passing a literal to 'mkVec' for the 'SInt' param disambiguates the length.
x1 :: IO ()
x1 = print $ mkVec 4 id

-- This also propagates to the 'Fin' indices: changing 2 to 4 is a type error.
x2 :: IO ()
x2 = print $ mkVec 4 id ! 2

-- Just driving the point home: 'fromList' works the same way.
x3 :: IO ()
x3 = print $ fromList 5 "aoeui" ! 4

-- Matching on 'svSize' makes the length index statically-known.
x4 :: Vec n a -> Maybe (Vec 4 a)
x4 v = case svSize v of 4 -> Just v; _ -> Nothing
