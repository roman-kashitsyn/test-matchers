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
{-# OPTIONS_HADDOCK prune #-}
{- |
Module:       Test.Matchers.Message
Description:  Types and functions to manipulate the matcher descriptions.
Copyright:    2018 Google LLC
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

This module provides basic type aliases and abstractions to construct
messages in matchers.

-}
module Test.Matchers.Message
  ( Message (..)
  , display
  , fancyChar
  , hcat
  , hsep
  , str
  , symbol
  ) where

import Data.String (IsString(..))
import Data.List (intersperse)

data Message
  = -- | Nothing at all
    Empty
  | -- | A single whitespace
    Space
  | -- | A string without any additional semantics.
    Str String
  | -- | A 'show'-ed value.
    Value String
  | -- | A symbol representing some entity, e.g. a function name.
    Symbol String
  | -- | A Unicode character with a replacement string if unicode is disabled.
    FancyChar Char String
  | -- | Vertical concatenation of messages.
    HCat [Message]
  deriving (Show, Eq, Ord)

instance Monoid Message where
  mempty = Empty

  mappend a Empty = a
  mappend Empty b = b
  mappend (Str x) (Str y) = Str (x ++ y)
  mappend (HCat xs) (HCat ys) = HCat (xs ++ ys)
  mappend a b = HCat [a, b]

instance IsString Message where
  fromString [] = Empty
  fromString s = Str s

notEmpty :: [Message] -> [Message]
notEmpty = filter (/= Empty)

hcat' :: [Message] -> Message
hcat' [] = Empty
hcat' xs = HCat xs

-- | Promotes the given string into a Message.
str :: String -> Message
str = Str

-- | Converts given value to a message and annotates it properly.
display :: (Show a) => a -> Message
display = Value . show

-- | Makes a unicode symbol that 
fancyChar :: Char -> String -> Message
fancyChar = FancyChar

-- | Catenates messages horizontally.
hcat :: [Message] -> Message
hcat = hcat' . notEmpty

hsep :: [Message] -> Message
hsep = hcat' . intersperse Space . notEmpty

-- | Encodes the given value as a symbol, typically the name of the
-- constructor or function.
symbol :: (Show a) => a -> Message
symbol = Symbol . show
