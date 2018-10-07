{-
Copyright 2018 Google LLC

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
{- |
Module:       Test.Matchers.HUnit
Description:  HUnit integration.
Copyright:    2018 Google LLC
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

This module contains functions that allow one to use matchers in HUnit
tests. Note that the same functions can be easily used in other test
frameworks, e.g. hspec or tasty.
-}
module Test.Matchers.HUnit
  ( shouldMatch
  , shouldMatchIO
  ) where

import Test.Matchers.Simple
import Test.Matchers.Render

import Control.Monad (unless)
import Data.Functor.Identity (Identity (..), runIdentity)
import System.Environment (lookupEnv)

import Test.HUnit (Assertion, assertFailure)

toColorMode :: Maybe String -> Mode
toColorMode Nothing  = RichText
toColorMode (Just s) = if  s `elem` ["", "1", "yes", "true", "color"]
                       then RichText
                       else PlainText

treeToAssertion :: MatchTree -> Assertion
treeToAssertion tree = unless (mtValue tree) $ do
                         env <- lookupEnv "TEST_MATCHERS_COLOR"
                         assertFailure $ prettyPrint
                           PPOptions { ppMode = toColorMode env } tree

-- | Checks that a pure value is matched by the given matcher.
-- The function is designed to be used in test frameworks, mainly HUnit and
-- HSpec. If the value doesn't satisfy the matcher, the test will fail and
-- print the detailed match tree.
--
-- > testCase = TestCase (fib 5 `shouldMatch` eq 5)
--
shouldMatch :: (Show a)
            => a          -- ^ a value to match
            -> Matcher a  -- ^ matcher to run
            -> Assertion
shouldMatch x m = treeToAssertion (match x m)

-- | A variant of 'shouldMatch' that matches an IO action instead of a
-- pure value.
--
-- > readNonExistingFile `shouldMatchIO` throws (anything :: Matcher IOException)
-- 
shouldMatchIO :: IO a          -- ^ an action to match
              -> MatcherF IO a -- ^ a matcher of the action
              -> Assertion
shouldMatchIO action matcher = matcher (Just action) >>= treeToAssertion
