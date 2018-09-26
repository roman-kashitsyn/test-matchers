{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{- |
Module:       Test.Matchers.Render
Description:  Functions for rendering match tree.
Copyright:    (c) Roman Kashitsyn, 2018
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
treeToMessage tree = case tryGetTrace tree of
                       Nothing  -> renderAsTree tree
                       (Just path) -> renderPath path

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
renderAsTree (Node res msg val subnodes) =
  annotate msgStyle (if res then check else cross) <+>
  if null subnodes
  then lineDoc
  else vsep [lineDoc, subtreeDoc]
  where msgStyle = if res then Success else Failure
        lineDoc = hsep [ annotate msgStyle msg
                       , arrow <+> val
                       ]
        subtreeDoc = indent 2 (vsep $ map treeToMessage subnodes)

-- | Renders the list of tree nodes as a stack trace.
renderPath :: [MatchTree] -> Message
renderPath path =
  case reverse path of
    [] -> error "Internal error: empty path"
    (reason:rest) -> if null rest
                     then renderReason reason
                     else vsep [ renderReason reason
                               , indent 2 $ renderRest rest
                               ]
  where renderReason node =
          vcat [ hsep [ fill 10 $ "Expected:"
                      , nodeMessage node
                      ]
               , hsep [ fill 10 $ "Got:"
                      , nodeMatchedValue node
                      ]
               ]
        renderRest = vsep . concatMap toEntry
        toEntry node = [ hsep ["in" , nodeMessage node]
                       , indent 3 $ hsep [ "Got:"
                                         , nodeMatchedValue node
                                         ]
                       ]

check, cross, arrow :: Message
check = "☑"
cross = "☒"
arrow = "←"

-- | Tries to find a single path in the tree that leads to the root
-- cause of the failure.
tryGetTrace :: MatchTree -> Maybe [MatchTree]
tryGetTrace node
  | nodeValue node = Nothing
  | null (nodeSubnodes node) = Just [node]
  | otherwise = case filter (not . nodeValue) (nodeSubnodes node) of
                  [e] -> fmap (node:) $ tryGetTrace e
                  _ -> Nothing

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
