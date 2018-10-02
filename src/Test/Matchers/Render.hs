-- Copyright 2018 Google LLC

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     https://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{- |
Module:       Test.Matchers.Render
Description:  Functions for rendering match tree.
Copyright:    2018 Google LLC
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

-}
module Test.Matchers.Render
  ( Mode(..)
  , PPOptions(..)
  , prettyPrint
  ) where


import Test.Matchers.Message
import Test.Matchers.Simple

import Data.Text.Lazy (unpack)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPT

-- | How to render the messages: use just plain text or fancy
-- decorations as well.
data Mode = PlainText | RichText
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Options controlling the pretty-printing.
data PPOptions
  = PPOptions
    { ppMode :: Mode
    }

-- | Renders the match tree as a document.  The function tries to be
-- pick the most readable format to represent the failure.
treeToMessage :: MatchTree -> Message
treeToMessage = renderAsTree

-- | Converts generic message styles into styles suitable for ANSI
-- terminal.
toAnsiStyle :: Style -> AnsiStyle
toAnsiStyle style = case style of
                      Plain -> mempty
                      Value -> PPT.italicized
                      Success -> PPT.bold <> PPT.color PPT.Red
                      Failure -> PPT.colorDull PPT.Green

-- | Renders the match tree as a textual tree: nested matchers have
-- higher indentation and the color indicates success or failure of
-- the matcher, i.e.
--
-- @
-- ☒ the root ← root
--   ☑ this node is OK ← good value
--   ☒ this node failed :( ← bad value
--     ☒ because this subnode failed. ← bad subvalue
-- @
renderAsTree :: MatchTree -> Message
renderAsTree (MatchTree res descr _ val subnodes) =
  annotate msgStyle (if res then check else cross) <+>
  if null subnodes
  then lineDoc
  else vsep [lineDoc, subtreeDoc]
  where msgStyle = if res then Success else Failure
        lineDoc = hsep [ annotate msgStyle descr
                       , arrow <+> val
                       ]
        subtreeDoc = indent 2 (vsep $ map renderAsTree subnodes)

check, cross, arrow :: Message
check = "✔"
cross = "✘"
arrow = "←"

-- | Pretty-prints the match tree according to the options provided.
prettyPrint
  :: PPOptions -- ^ Pretty-printing options.
  -> MatchTree -- ^ Match tree to format.
  -> String
prettyPrint opts = unpack . render . PP.reAnnotate toAnsiStyle .
                   applyMode (ppMode opts) . treeToMessage
  where render = PPT.renderLazy . PP.layoutPretty PP.defaultLayoutOptions
        applyMode PlainText = PP.unAnnotate
        applyMode RichText = id
