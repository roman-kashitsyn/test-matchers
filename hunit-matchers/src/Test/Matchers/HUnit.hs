{-
Copyright 2018-2019 Google LLC

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    https://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}
{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ImplicitParams #-}
{- |
Module:       Test.Matchers.HUnit
Description:  HUnit integration for Test.Matchers.
Copyright:    2018-2019 Google LLC
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

This module contains functions that allow one to use matchers in HUnit
tests. Note that the same functions can be easily used in other test
frameworks, e.g. hspec or tasty.
-}
module Test.Matchers.HUnit
  ( shouldMatch
  , shouldNotMatch
  , shouldMatchIO
  , shouldNotMatchIO
  ) where

import qualified Test.Matchers.HUnit.Implicit as I

import Test.HUnit (Assertion)
import Test.Matchers.Simple (Matcher, MatcherF)

-- | Checks that a pure value is matched by the given matcher.
-- The function is designed to be used in test frameworks, mainly HUnit and
-- HSpec. If the value doesn't satisfy the matcher, the test will fail and
-- print the detailed match tree.
--
-- > testCase = TestCase (fib 5 `shouldMatch` eq 5)
--
shouldMatch
  :: a -- ^ The value that should pass the test.
  -> Matcher a  -- ^ The matcher to run.
  -> Assertion
shouldMatch = let ?matchersOptionsAction = I.getOptionsFromEnv
              in I.shouldMatch

-- | The complement of 'shouldMatch'.
shouldNotMatch
  :: a -- ^ The value that should fail the test.
  -> Matcher a -- ^ The matcher to run.
  -> Assertion
shouldNotMatch = let ?matchersOptionsAction = I.getOptionsFromEnv
                 in I.shouldNotMatch

-- | A variant of 'shouldMatch' that matches an IO action instead of a
-- pure value.
--
-- > readNonExistingFile `shouldMatchIO` throws (anything :: Matcher IOException)
--
shouldMatchIO
  :: IO a -- ^ The action that should pass the test.
  -> MatcherF IO a -- ^ The matcher to run.
  -> Assertion
shouldMatchIO = let ?matchersOptionsAction = I.getOptionsFromEnv
                in I.shouldMatchIO

-- | The complement of 'shouldMatchIO'.
shouldNotMatchIO
  :: IO a -- ^ The action that should fail the test.
  -> MatcherF IO a -- ^ The matcher to run.
  -> Assertion
shouldNotMatchIO = let ?matchersOptionsAction = I.getOptionsFromEnv
                   in I.shouldNotMatchIO
