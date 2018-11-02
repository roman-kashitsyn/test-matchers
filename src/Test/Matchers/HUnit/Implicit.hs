{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ImplicitParams #-}
{- |
Module:       Test.Matchers.HUnit.Implicit
Description:  HUnit integration.
Copyright:    2018 Google LLC
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

This module contains functions that allow one to use matchers in HUnit
tests. All the functions provided by this module take one implicit
parameter — the pretty-printing configuration.


-}
module Test.Matchers.HUnit.Implicit
  ( shouldMatch
  , shouldNotMatch
  , shouldMatchIO
  , shouldNotMatchIO
  ) where

import Test.Matchers.Simple
import Test.Matchers.Render

import Control.Monad (unless)
import System.Environment (lookupEnv)

import Test.HUnit (Assertion, assertFailure)

treeToAssertion :: PPOptions -> MatchTree -> Assertion
treeToAssertion opts tree =
  unless (mtValue tree) (assertFailure $ prettyPrint opts tree)

shouldMatch
  :: (?matchersOptions :: PPOptions)
  => a -- ^ The value that should pass the test.
  -> Matcher a  -- ^ The matcher to run.
  -> Assertion
shouldMatch x m = treeToAssertion ?matchersOptions (match x m)

shouldNotMatch
  :: (?matchersOptions :: PPOptions)
  => a -- ^ The value that should fail the test.
  -> Matcher a -- ^ The matcher to run.
  -> Assertion
shouldNotMatch x m = shouldMatch x (negationOf m)

shouldMatchIO
  :: (?matchersOptions :: PPOptions)
  => IO a -- ^ The action that should pass the test.
  -> MatcherF IO a -- ^ The matcher to run.
  -> Assertion
shouldMatchIO action matcher = runMatcher matcher action >>= treeToAssertion ?matchersOptions

shouldNotMatchIO
  :: (?matchersOptions :: PPOptions)
  => IO a -- ^ The action that should fail the test.
  -> MatcherF IO a -- ^ The matcher to run.
  -> Assertion
shouldNotMatchIO action matcher = shouldNotMatchIO action (negationOf matcher)
