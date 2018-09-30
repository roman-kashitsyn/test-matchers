-- Copyright 2018 Google LLC

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     https://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Test.Matchers
  ( -- * Basic matchers
    module Test.Matchers.Simple
  , -- * HUnit integration
    module Test.Matchers.HUnit
  , module Test.Matchers.Message
  , module Test.Matchers.Render
  ) where

import Test.Matchers.Message (Message)
import Test.Matchers.Simple
import Test.Matchers.Render
import Test.Matchers.HUnit
