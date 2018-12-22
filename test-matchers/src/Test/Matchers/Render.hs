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

import Test.Matchers.Message (Message(..))
import Test.Matchers.Simple
import qualified Test.Matchers.Message as MSG

import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.List (intersperse)
import Data.Text.Lazy (unpack)
import Data.Text.Prettyprint.Doc ((<>), (<+>), Doc)
import Data.Text.Prettyprint.Doc.Render.Terminal (AnsiStyle)
import qualified Data.Text.Prettyprint.Doc as PP
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
  | SymbolStyle -- ^ The style to use for printing symbols.
  | RefStyle -- ^ The style to use for references.
  | SuccessStyle -- ^ The style to use for successfully completed matchers.
  | FailureStyle -- ^ The style to use for failed matchers.
  deriving (Eq, Show, Enum, Bounded)


-- | Options controlling the pretty-printing.
data PPOptions
  = PPOptions
    { ppMode :: !Mode -- ^ The mode specifies whether the medium
                      -- supports fancy styles and colors.
    , ppUseUnicode :: !Bool -- ^ Whether we're allowed to use fancy
                            -- Unicode symbols in the output.
    , ppMaxValueWidth :: !Int -- ^ Max length of the value before
                              -- it's turned into a reference.
    , ppPageWidth :: !Int -- ^ Maximum page width in characters for
                          -- the pretty printer.
    } deriving (Eq, Show)

-- | Default values for the 'PPOptions'.
defaultPPOptions :: PPOptions
defaultPPOptions = PPOptions
                   { ppMode = RichText
                   , ppUseUnicode = True
                   , ppMaxValueWidth = 20
                   , ppPageWidth = 80
                   }

-- Minimal implementation of the (lazy) State + Reader monad to avoid
-- the direct dependency on transformers.

newtype RState r s a = RState { runRState :: r -> s -> (a, s) }

instance Functor (RState r s) where
  fmap f (RState run) = RState $ \r s -> let (a, s') = run r s in (f a, s')

instance Applicative (RState r s) where
  pure x = RState $ \_ s -> (x, s)
  (RState runF) <*> (RState runX) = RState $ \r s ->
                                               let (f, s') = runF r s
                                                   (x, s'') = runX r s'
                                               in (f x, s'')

instance Monad (RState r s) where
  return = pure
  (RState run) >>= f = RState $ \r s -> let (x, s') = run r s
                                        in runRState (f x) r s'

get :: RState r s s
get = RState $ \_ s -> (s, s)

put :: s -> RState r s ()
put s = RState $ \_ _ -> ((), s)

ask :: RState r s r
ask = asks id

asks :: (r -> r') -> RState r s r'
asks view = RState $ \r s -> (view r, s)

-- end of the State implementation

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
    renderRef (refId, val) = displayRef refId <+> PP.pretty val

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

messageToDoc :: PPOptions -> Message -> Doc Style
messageToDoc opts msg
  = case msg of
      Empty -> mempty
      Space -> PP.space
      Str s -> PP.pretty s
      Value v -> PP.annotate ValueStyle $ PP.pretty v
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
              descrDoc = messageToDoc opts (displayLabels labels descr)
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
