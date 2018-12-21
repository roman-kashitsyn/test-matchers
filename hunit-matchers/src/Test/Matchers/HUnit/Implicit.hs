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
{-# LANGUAGE ImplicitParams #-}
{- |
Module:       Test.Matchers.HUnit.Implicit
Description:  HUnit integration for Test.Matchers: the interface with implicit parameters.
Copyright:    2018 Google LLC
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

This module contains functions that allow one to use matchers in HUnit
tests. All the "shouldMatch" functions provided by this module take
one implicit parameter — an action returning the pretty-printing
configuration to use in case of a test failure. Note that the action
will only be executed if the test fails.

-}
module Test.Matchers.HUnit.Implicit
  (
  -- * Heuristics for capabilities detection
    getOptionsFromEnv
  , optionsFromEnv
  -- * Runners for matchers
  , shouldMatch
  , shouldNotMatch
  , shouldMatchIO
  , shouldNotMatchIO
  ) where

import Control.Monad (unless)
import Data.Char (toLower)
import Data.List (isInfixOf)
import System.Environment (getEnvironment)
import Test.HUnit (Assertion, assertFailure)
import Test.Matchers.Simple
  ( Matcher
  , MatcherF
  , MatchTree
  , match
  , mtValue
  , negationOf
  , runMatcher
  )
import Test.Matchers.Render
  ( Mode(PlainText)
  , PPOptions
  , defaultPPOptions
  , ppMode
  , ppUseUnicode
  , prettyPrint
  )

-- | Adjusts the options based on the TERM environment variable.
applyTerm :: Maybe String -> PPOptions -> PPOptions
applyTerm (Just term) opts | isKnownDumb term = opts { ppMode = PlainText }
  where isKnownDumb t = t == "dumb" || "emacs" `isInfixOf` t
applyTerm _ opts = opts -- Assume most terminals support ANSI escape codes

-- | Adjusts the options based on the LANG environment variable.
applyLang :: Maybe String -> PPOptions -> PPOptions
applyLang (Just lang) opts | hasUnicode lang = opts
  where hasUnicode l = "utf" `isInfixOf` map toLower l
applyLang _ opts = opts { ppUseUnicode = False }

-- | Deduces pretty-printing options from the given environment.
-- It's mostly based on heuristics, use with care.
optionsFromEnv :: [(String, String)] -> PPOptions
optionsFromEnv env =
  applyTerm (lookup "TERM" env)
  . applyLang (lookup "LANG" env)
  $ defaultPPOptions

-- | Wrapper around 'optionsFromEnv' that reads the environment of the
-- current process.
getOptionsFromEnv :: IO PPOptions
getOptionsFromEnv = optionsFromEnv <$> getEnvironment

treeToAssertion :: IO PPOptions -> MatchTree -> Assertion
treeToAssertion optsAction tree =
  unless (mtValue tree) $ do
    opts <- optsAction
    assertFailure $ prettyPrint opts tree

shouldMatch
  :: (?matchersOptionsAction :: IO PPOptions)
  => a -- ^ The value that should pass the test.
  -> Matcher a  -- ^ The matcher to run.
  -> Assertion
shouldMatch x m = treeToAssertion ?matchersOptionsAction (match x m)

shouldNotMatch
  :: (?matchersOptionsAction :: IO PPOptions)
  => a -- ^ The value that should fail the test.
  -> Matcher a -- ^ The matcher to run.
  -> Assertion
shouldNotMatch x m = shouldMatch x (negationOf m)

shouldMatchIO
  :: (?matchersOptionsAction :: IO PPOptions)
  => IO a -- ^ The action that should pass the test.
  -> MatcherF IO a -- ^ The matcher to run.
  -> Assertion
shouldMatchIO action matcher =
  runMatcher matcher action >>= treeToAssertion ?matchersOptionsAction

shouldNotMatchIO
  :: (?matchersOptionsAction :: IO PPOptions)
  => IO a -- ^ The action that should fail the test.
  -> MatcherF IO a -- ^ The matcher to run.
  -> Assertion
shouldNotMatchIO action matcher =
  shouldNotMatchIO action (negationOf matcher)
