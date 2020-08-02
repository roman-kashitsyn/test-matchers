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
Module:       LexShow
Description:  A lexer for 'show'-ed values.
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
{-# LANGUAGE TupleSections #-}
module Test.Matchers.LexShow
  ( Token
  , TokType(..)
  , LexResult(..)
  , DisplayLimit
  , resultTokens
  , mapLexResult
  , tokenType
  , tokenText
  , lexShow
  , refToToken
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
  deriving (Eq, Ord, Show)

data Token
  = Token
  { tokenType :: !TokType
  , tokenText :: !Text
  } deriving (Eq, Show, Ord)

data TokType
  = TokNumber
  | TokChar
  | TokString
  | TokIdent
  | TokSymbol
  | TokPunct
  | TokSpace
  | TokRef
  | TokUnknown
  deriving (Show, Eq, Ord, Enum)

type DisplayLimit = Int

annot :: TokType -> Machine a -> Machine (TokType, a)
annot tok = fmap (tok,)

digit, octit, hexit :: Machine Range
digit = satisfying isDigit
octit = satisfying isOctDigit
hexit = satisfying isHexDigit

number :: Machine (TokType, Range)
number = annot TokNumber $ float <|> decimal <|> octal <|> hex

decimal, float, octal, hex :: Machine Range
decimal = foldMany1 digit
float = decimal <> char '.' <> decimal <> opt (charCase 'e' <> opt (oneOfChars "+-") <> decimal)
octal = str "0o" <> foldMany1 octit
hex = str "0x" <> foldMany1 hexit

data StrState
  = StrStart
  | StrInside
  | StrEscape
  | StrEnd
  deriving (Eq, Show)

string :: Machine (TokType, Range)
string = (\(_, r) -> (TokString, r)) <$> scan (StrStart, mempty) strScanner
  where strS StrStart '"' = Just StrInside
        strS StrStart _ = Nothing
        strS StrInside '"' = Just StrEnd
        strS StrInside '\\' = Just StrEscape
        strS StrInside _ = Just StrInside
        strS StrEscape _ = Just StrInside
        strS StrEnd _ = Nothing
        strScanner (s, r) n c = (,r <> singleton n) <$> strS s c

ident :: Machine (TokType, Range)
ident = annot TokIdent $ satisfying isFirstIdChar <> foldMany (satisfying isIdChar)
  where isFirstIdChar c = isAlpha c || c == '_'
        isIdChar c = isAlphaNum c || c == '_' || c == '\''

punct :: Machine (TokType, Range)
punct = annot TokPunct $ oneOfChars punctChars <|> oneOfStr reserved

reserved :: [String]
reserved = ["..", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

punctChars :: String
punctChars = ",;()[]{}`"

punctStarts :: String
punctStarts = punctChars ++ map head reserved

symbol :: Machine (TokType, Range)
symbol = annot TokSymbol $ foldMany1 (satisfying isSymbolChar)

fallback :: Machine (TokType, Range)
fallback = annot TokUnknown $ foldMany (satisfying isWordChar)
  where isWordChar c = not (isPunctuation c || isSpace c || c `elem` punctStarts)

space :: Machine (TokType, Range)
space = annot TokSpace $ foldMany1 (satisfying isSpace)

chr :: Machine Range
chr = normal <|> escape
  where normal = satisfying (\c -> c /= '\'' && c /= '\\')

charLit :: Machine (TokType, Range)
charLit = annot TokChar $ ch' <> chr <> ch'
  where ch' = char '\''

escape :: Machine Range
escape = char '\\' <> (charesc <|> ascii <|> decimal)
  where charesc = oneOfChars "abfnrtv\\\"'&"

ascii :: Machine Range
ascii = oneOfStr [ "ACK"
                 , "BEL"
                 , "BS"
                 , "CAN"
                 , "CR"
                 , "DC1"
                 , "DC2"
                 , "DC3"
                 , "DC4"
                 , "DEL"
                 , "DLE"
                 , "EM"
                 , "ENQ"
                 , "EOT"
                 , "ESC"
                 , "ETB"
                 , "ETX"
                 , "FF"
                 , "FS"
                 , "GS"
                 , "HT"
                 , "LF"
                 , "NAK"
                 , "NUL"
                 , "RS"
                 , "SI"
                 , "SO"
                 , "SOH"
                 , "SP"
                 , "STX"
                 , "SUB"
                 , "SYN"
                 , "US"
                 , "VT"
                 ]
        <|> char '^' <> (oneOfChars "@[\\]^_" <|> charRange 'A' 'Z')

token :: Machine (TokType, Range)
token = ident
        <|> punct
        <|> charLit
        <|> number
        <|> string
        <|> symbol
        <|> space
        <|> fallback

tokenize :: Text -> [Token]
tokenize text = go (T.length text) text
  where
    substr (Range b e) = T.take (e - b) . T.drop b
    go n t =
      if T.null t
      then []
      else case runMachineText token t of
             (len, _, _) | len == 0 -> error $ "Lexer stuck on " ++ show t
             (len, Done (tok, range), t') -> let tokT = substr range t
                                             in Token tok tokT : go (n - len) t'
             (len, _, t') -> Token TokUnknown (T.take len t) : go (n - len) t'

lexShow :: DisplayLimit -> String -> LexResult
lexShow limit showed = case splitAt limit showed of
                         (prefix, []) -> Full $ tokenize $ T.pack prefix
                         (prefix, _) -> Partial $ tokenize $ T.pack prefix

-- | Creates a token that is a reference to another object.  We chose
-- to display the references as "<n>".
refToToken :: Int -> Token
refToToken refId = Token TokRef $ T.pack $ "<" ++ show refId ++ ">"

resultTokens :: LexResult -> [Token]
resultTokens (Full toks) = toks
resultTokens (Partial toks) = toks

mapLexResult :: ([Token] -> [Token]) -> LexResult -> LexResult
mapLexResult f (Full tokens) = Full (f tokens)
mapLexResult f (Partial tokens) = Partial (f tokens)
