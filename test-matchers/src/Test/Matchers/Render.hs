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
  , defaultPPOptions
  , prettyPrint
  ) where

import RStateMonad (RState, ask, get, put, runRState)
import Test.Matchers.Message (Message(..))
import Test.Matchers.PrettyShow (prettyShow)
import Test.Matchers.LexShow (TokType(..))
import qualified Test.Matchers.Message as MSG
import Test.Matchers.Simple

import qualified Data.IntMap as IM
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Text.Lazy (unpack)
import Data.Text.Prettyprint.Doc (Doc, (<+>), (<>))
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PPT

-- | How to render the messages: use just plain text or fancy
-- decorations as well.
data Mode = PlainText | RichText
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Abstract styles used by the library. The actual representation is
-- controlled by the rendering.
data Style
  = PlainStyle -- ^ Default style of the output.
  | ValueStyle -- ^ The style to use for printing values having a Show instance.
  | TokenStyle TokType -- ^ The style to use to print a token.
  | SymbolStyle -- ^ The style to use for printing symbols.
  | RefStyle -- ^ The style to use for references.
  | SuccessStyle -- ^ The style to use for successfully completed matchers.
  | FailureStyle -- ^ The style to use for failed matchers.
  deriving (Eq, Show)

-- | Options controlling the pretty-printing.
data PPOptions
  = PPOptions
    { ppMode :: !Mode -- ^ The mode specifies whether the medium
                      -- supports fancy styles and colors.
    , ppUseUnicode :: !Bool -- ^ Whether we're allowed to use fancy
                            -- Unicode symbols in the output.
    , ppMaxValueWidth :: !Int -- ^ Max length of the value before
                              -- it's turned into a reference.
    , ppMaxShowLength :: !Int -- ^ The maximal length of the showed value.
    , ppPageWidth :: !Int -- ^ Maximum page width in characters for
                          -- the pretty printer.
    } deriving (Eq, Show)

-- | Default values for the 'PPOptions'.
defaultPPOptions :: PPOptions
defaultPPOptions = PPOptions
                   { ppMode = RichText
                   , ppUseUnicode = True
                   , ppMaxValueWidth = 20
                   , ppMaxShowLength = 2000
                   , ppPageWidth = 80
                   }

type RenderState = RState PPOptions (M.Map String Int)

-- | Renders the match tree as a document.  The function tries to be
-- pick the most readable format to represent the failure.
treeToMessage :: PPOptions -> MatchTree -> Doc Style
treeToMessage = renderAsTree

-- | Converts generic message styles into styles suitable for ANSI
-- terminal.
toAnsiStyle :: Style -> AnsiStyle
toAnsiStyle style = case style of
                      PlainStyle -> mempty
                      ValueStyle -> PPT.italicized
                      SymbolStyle -> PPT.italicized
                      RefStyle -> PPT.bold
                      SuccessStyle -> PPT.colorDull PPT.Green
                      FailureStyle -> PPT.bold <> PPT.colorDull PPT.Red
                      TokenStyle tok -> tokToAnsiStyle tok
  where tokToAnsiStyle tok = case tok of
                               TokNumber -> PPT.colorDull PPT.Red
                               TokString -> PPT.colorDull PPT.Green
                               TokIdent -> PPT.italicized
                               _ -> mempty

-- | Renders the match tree as a textual tree: nested matchers have
-- higher indentation and the color indicates success or failure of
-- the matcher, i.e.
--
-- @
-- ✘ the root ← <1>
--   ✔ this node is OK ← good value
--   ✘ this node failed :( ← bad value
--     ✘ because this subnode failed. ← bad subvalue
-- where:
--   <1> big root object
-- @
--
-- Values that are too long are represented as references and are
-- printed in the bottom of the tree.
renderAsTree :: PPOptions -> MatchTree -> Doc Style
renderAsTree opts t =
  if M.null refs
  then doc
  else doc <> PP.hardline <> "where:" <> PP.hardline <> PP.indent 2 (renderRefs refs)
  where
    (doc, refs) = runRState (renderAsTreeWithRefs t) opts M.empty
    swap (x, y) = (y, x)
    renderRefs = PP.vsep . map renderRef . IM.toList . IM.fromList . map swap . M.toList
    renderRef (refId, val) = displayRef refId <+> displayValue opts val

allocateId :: String -> RenderState Int
allocateId msg = do
  m <- get
  case M.lookup msg m of
    Just i -> return i
    Nothing -> do
      let n = 1 + M.size m
      put $ M.insert msg n m
      return n

lengthWithinLimit :: Int -> String -> Bool
lengthWithinLimit n s = case splitAt n s of
                          (_, []) -> True
                          _ -> False

displayValue :: PPOptions -> String -> Doc Style
displayValue opts = PP.reAnnotate TokenStyle . prettyShow (ppMaxShowLength opts)

messageToDoc :: PPOptions -> Message -> Doc Style
messageToDoc opts msg
  = case msg of
      Empty -> mempty
      Space -> PP.space
      Str s -> PP.pretty s
      Value v -> displayValue opts v
      Symbol s -> PP.annotate SymbolStyle $ PP.pretty s
      FancyChar c s -> if ppUseUnicode opts then PP.pretty c else PP.pretty s
      HCat ms -> PP.hcat $ map (messageToDoc opts) ms

renderAsTreeWithRefs :: MatchTree -> RenderState (Doc Style)
renderAsTreeWithRefs (MatchTree res descr labels val subnodes) = do
  doc <- lineDoc
  subtreeDocs <- traverse renderAsTreeWithRefs subnodes
  opts <- ask
  let subtreeDoc = PP.indent 2 (PP.vsep subtreeDocs)
  let prefix = PP.annotate msgStyle $ messageToDoc opts $ if res then check else cross
  return $  prefix <+> if null subnodes then doc else PP.vsep [doc, subtreeDoc]
  where msgStyle = if res then SuccessStyle else FailureStyle
        lineDoc = do
          opts <- ask
          let limit = ppMaxValueWidth opts
              arrowDoc = messageToDoc opts arrow
              descrDoc = PP.annotate msgStyle $ messageToDoc opts (displayLabels labels descr)
          case val of
            Nothing -> return descrDoc
            Just valMsg | not (lengthWithinLimit limit valMsg) -> do
                            refId <- allocateId valMsg
                            return $ PP.hsep [descrDoc, arrowDoc, displayRef refId]
            Just valMsg -> return $ PP.hsep [descrDoc, arrowDoc, PP.pretty valMsg]

displayLabels :: [String] -> Message -> Message
displayLabels [] m = m
displayLabels xs m = mconcat
                     [ MSG.str "["
                     , MSG.hcat (intersperse compose $ map MSG.str xs)
                     , MSG.str "]"
                     , MSG.space
                     , m
                     ]

compose, check, cross, arrow :: Message
check = MSG.fancyChar '✔' "[v]"
cross = MSG.fancyChar '✘' "[x]"
arrow = MSG.fancyChar '←' "<-"
compose = MSG.str "."

displayRef :: Int -> Doc Style
displayRef ref = PP.annotate RefStyle $ PP.hcat ["<", PP.pretty ref, ">"]

-- | Pretty-prints the match tree according to the options provided.
prettyPrint
  :: PPOptions -- ^ Pretty-printing options.
  -> MatchTree -- ^ Match tree to format.
  -> String
prettyPrint opts = unpack . render . PP.reAnnotate toAnsiStyle .
                   applyMode (ppMode opts) . treeToMessage opts
  where render = PPT.renderLazy . PP.layoutPretty ppOpts
        applyMode PlainText = PP.unAnnotate
        applyMode RichText = id
        ppOpts = PP.defaultLayoutOptions {PP.layoutPageWidth = PP.AvailablePerLine (ppPageWidth opts) 1.0}
