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
{- |
Module:       Test.Matchers.Tasty
Description:  Tasty integration for Test.Matchers.
Copyright:    2018 Google LLC
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

This module provides an integration layer required in order to use
Test.Matchers library with the Tasty library.
-}
{-# LANGUAGE DeriveDataTypeable #-}
module Test.Matchers.Tasty
  ( MatchersTest
  , shouldMatch
  , shouldNotMatch
  , shouldMatchIO
  , shouldNotMatchIO
  , testCase
  ) where

import Data.Typeable (Typeable)
import Data.Proxy (Proxy(Proxy))
import Test.Matchers
  ( MatchTree
  , Matcher
  , MatcherF
  , match
  , negationOf
  , runMatcher
  , mtValue
  )
import Test.Matchers.Render
  ( Mode(PlainText, RichText)
  , PPOptions(..)
  , defaultPPOptions
  , prettyPrint
  )
import Test.Tasty.Options
  ( OptionSet
  , OptionDescription (Option)
  , lookupOption
  )
import Test.Tasty.Ingredients.ConsoleReporter
  ( UseColor(Never, Always, Auto))
import Test.Tasty.Providers
  ( IsTest(run, testOptions)
  , TestTree
  , testPassed
  , testFailed
  , singleTest
  )

newtype MatchersTest
  = MatchersTest { getTree :: IO MatchTree }
  deriving (Typeable)

instance IsTest MatchersTest where
  testOptions = return [ Option (Proxy :: Proxy UseColor) ]
  run optionSet t _progress = do
    tree <- getTree t
    if mtValue tree
      then pure $ testPassed ""
      else do
        let ppOpts = applyOptions optionSet defaultPPOptions
        return $ testFailed (prettyPrint ppOpts tree)
    
applyOptions :: OptionSet -> PPOptions -> PPOptions
applyOptions optSet opts =
  case useColor of
    Never -> opts { ppMode = PlainText }
    Always -> opts { ppMode = RichText }
    Auto -> opts
  where useColor = lookupOption optSet

testCase :: String -> MatchersTest -> TestTree
testCase = singleTest

shouldMatch :: a -> Matcher a -> MatchersTest
shouldMatch v m = MatchersTest $ pure (match v m)

shouldNotMatch :: a -> Matcher a -> MatchersTest
shouldNotMatch v m = shouldMatch v (negationOf m)

shouldMatchIO :: IO a -> MatcherF IO a -> MatchersTest
shouldMatchIO v m = MatchersTest (runMatcher m v)

shouldNotMatchIO :: IO a -> MatcherF IO a -> MatchersTest
shouldNotMatchIO v m = shouldMatchIO v (negationOf m)

