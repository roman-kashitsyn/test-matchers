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
{- |
Module:       Test.Matchers.QuickCheck
Description:  QuickCheck integration for Test.Matchers.
Copyright:    2018-2019 Google LLC
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

This module contains 'Testable' orphan instances turning matchers into
testable properties.
-}
{-# LANGUAGE FlexibleInstances #-}
module Test.Matchers.QuickCheck where

import Data.Functor.Identity (Identity)
import Test.Matchers.Render (defaultPPOptions, prettyPrint)
import Test.Matchers.Simple (MatchTree, MatcherF, match, mtValue, runMatcher)
import Test.QuickCheck
  ( Arbitrary
  , Testable(property)
  , counterexample
  , idempotentIOProperty
  )

instance Testable MatchTree where
  property tree = counterexample (prettyPrint defaultPPOptions tree)
                  $ property (mtValue tree)

instance (Arbitrary a, Show a) => Testable (MatcherF Identity a) where
  property matcher = property $ \x -> match matcher x

instance (Arbitrary a, Show a) => Testable (MatcherF IO a) where
  property matcher = property $ \x -> idempotentIOProperty (runMatcher matcher (pure x))
