{-
Copyright 2018-2019 Google LLC
Copyright 2020 Roman Kashitsyn

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
{-
|
Module:       Test.Matchers.PrettyShow
Description:  Pretty-print 'show'-ed values.
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental


-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Matchers.PrettyShow
  ( prettyShow
  , prettyShowResult
  , prettyRef
  ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc)
import qualified Data.Text.Prettyprint.Doc as PP
import Test.Matchers.LexShow
  ( DisplayLimit
  , LexResult(..)
  , TokType(..)
  , Token(..)
  , lexShow
  , refToToken
  )

tokenToDoc :: Token -> Doc TokType
tokenToDoc tok = PP.annotate (tokenType tok) $ PP.pretty (tokenText tok)

etc :: Doc TokType
etc = PP.pretty ("..." :: Text)

-- | Pretty-prints a string representation of a value.
prettyShow :: DisplayLimit -> String -> Doc TokType
prettyShow n s = prettyShowResult (lexShow n s)

prettyShowResult :: LexResult -> Doc TokType
prettyShowResult (Full tokens) = PP.hcat (map tokenToDoc tokens)
prettyShowResult (Partial tokens) = PP.hcat (map tokenToDoc tokens) PP.<> etc

prettyRef :: Int -> Doc TokType
prettyRef = tokenToDoc . refToToken
