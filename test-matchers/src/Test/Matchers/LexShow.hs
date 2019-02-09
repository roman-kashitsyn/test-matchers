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
{-
|
Module:       LexShow
Description:  A lexer for 'show'-ed values.
Copyright:    2018 Google LLC
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

This module contains a lexer for outputs of the 'show' function. It's
different from 'Text.Read.Lex' in two aspects:

  * It never fails, it can always find some interpretation of the
    input.

  * It always produces finite output, even if the input stream is
    infinite. If the stream is longer than the specified limit, a
    special token is produced by the lexer and the lexing terminates.
-}
module Test.Matchers.LexShow
  ( Token
  , TokType(..)
  , LexResult(..)
  , tokenType
  , tokenText
  , lexShow
  ) where

import Machines

import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T

import Data.Char
  ( isAlpha
  , isAlphaNum
  , isDigit
  , isHexDigit
  , isOctDigit
  , isPunctuation
  , isSpace
  )
import Data.Monoid ((<>))
import Text.Read.Lex (isSymbolChar)

data LexResult
  = Full [Token]
  | Partial [Token]
  deriving (Eq, Show)

data Token
  = Token
  { tokenType :: !TokType
  , tokenText :: !Text
  } deriving (Eq, Show, Ord)

data TokType
  = TokNumber
  | TokString
  | TokIdent
  | TokSymbol
  | TokPunct
  | TokSpace
  | TokUnknown
  deriving (Show, Eq, Ord, Enum)

annot :: TokType -> Machine a -> Machine (TokType, a)
annot tok = fmap (\x -> (tok, x))

digit, octit, hexit :: Machine Range
digit = satisfying isDigit
octit = satisfying isOctDigit
hexit = satisfying isHexDigit

number :: Machine (TokType, Range)
number = annot TokNumber (float <|> decimal <|> octal <|> hex)

decimal, float, octal, hex :: Machine Range
decimal = foldMany1 digit
float = decimal <> char '.' <> decimal <> opt (charCase 'e' <> (opt $ oneOfChars "+-") <> decimal)
octal = str "0o" <> foldMany1 octit
hex = str "0x" <> foldMany1 hexit

data StrState
  = StrStart
  | StrInside
  | StrEscape
  | StrEnd
  deriving (Eq, Show)

string :: Machine (TokType, Range)
string = fmap (\(_, r) -> (TokString, r)) $ scan (StrStart, mempty) strScanner
  where strS StrStart '"' = Just StrInside
        strS StrStart _ = Nothing
        strS StrInside '"' = Just StrEnd
        strS StrInside '\\' = Just StrEscape
        strS StrInside _ = Just StrInside
        strS StrEscape _ = Just StrInside
        strS StrEnd _ = Nothing
        strScanner (s, r) n c = fmap (\s' -> (s', r <> singleton n)) $ strS s c

ident :: Machine (TokType, Range)
ident = annot TokIdent $ satisfying isFirstIdChar <> foldMany (satisfying isIdChar)
  where isFirstIdChar c = isAlpha c || c == '_'
        isIdChar c = isAlphaNum c || c == '_' || c == '\''

punct :: Machine (TokType, Range)
punct = annot TokPunct $ oneOfChars punctChars <|> foldr1 (<|>) (map str reserved)

reserved :: [String]
reserved = ["..", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

punctChars :: String
punctChars = ",;()[]{}`"

punctStarts :: String
punctStarts = punctChars ++ map head reserved

symbol :: Machine (TokType, Range)
symbol = annot TokSymbol $ foldMany1 (satisfying isSymbolChar)

fallback :: Machine (TokType, Range)
fallback = annot TokUnknown $ foldMany1 (satisfying isWordChar)
  where isWordChar c = not (isPunctuation c || isSpace c || c `elem` punctStarts)

space :: Machine (TokType, Range)
space = annot TokSpace $ foldMany1 (satisfying isSpace)

token :: Machine (TokType, Range)
token = ident
        <|> punct
        <|> number
        <|> string
        <|> symbol
        <|> space
        <|> fallback

tokenize :: Text -> [Token]
tokenize text = let trimmed = T.strip text
                in go (T.length trimmed) trimmed
  where
    substr (Range b e) = T.take (e - b) . T.drop b
    go n t =
      if T.null t
      then []
      else case runMachineText token t of
             (len, Done (tok, range), t') -> let tokT = substr range t
                                             in Token tok tokT : go (n - len) t'
             (len, _, t') -> Token TokUnknown (T.take len t) : go (n - len) t'

lexShow :: Int -> String -> LexResult
lexShow limit showed = case splitAt limit showed of
                         (prefix, []) -> Full $ tokenize $ T.pack prefix
                         (prefix, _) -> Partial $ tokenize $ T.pack prefix
