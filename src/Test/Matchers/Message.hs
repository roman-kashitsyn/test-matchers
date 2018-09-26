{-# OPTIONS_HADDOCK prune #-}
{- |
Module:       Test.Matchers.Message
Description:  Types and functions to manipulate the matcher descriptions.
Copyright:    (c) Roman Kashitsyn, 2018
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

This module provides basic type aliases and abstractions to construct
messages in matchers.

-}
module Test.Matchers.Message
  ( Message
  , Style(..)
  , display
  , (<+>)
  , (<>)
  , annotate
  , hcat
  , hsep
  , vcat
  , vsep
  , indent
  , fill
  , pretty
  ) where

import Data.Text.Prettyprint.Doc

-- | Abstract styles used by the library. The actual representation is
-- controlled by the rendering.
data Style
  = Plain -- ^ Default style of the output
  | Value -- ^ The style to use for printing values having Show instances
  | Success -- ^ The style to use for successfully completed matchers
  | Failure -- ^ The style to use for failed matchers
  deriving (Eq, Show, Enum, Bounded)

-- | Message is a pretty-printable document annotated with style.
type Message = Doc Style

-- | Converts given value to message and annotates it properly.
display :: (Show a) => a -> Message
display = annotate Value . pretty . show
