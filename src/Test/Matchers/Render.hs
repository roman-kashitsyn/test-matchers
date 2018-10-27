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
  , prettyPrint
  ) where

import Test.Matchers.Message
import Test.Matchers.Simple

import qualified Data.Map as M
import qualified Data.IntMap as IM
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

-- Minimal implementation of the (lazy) State monad to avoid the
-- direct dependency on transformers.

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State run) = State $ \s -> let (a, s') = run s in (f a, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  (State runF) <*> (State runX) = State $ \s ->
                                            let (f, s') = runF s
                                                (x, s'') = runX s'
                                            in (f x, s'')

instance Monad (State s) where
  return = pure
  (State run) >>= f = State $ \s -> let (x, s') = run s
                                    in runState (f x) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- end of the State implementation

newtype DisplayMsg = DisplayMsg { unDisplay :: Message } deriving (Show)

instance Eq DisplayMsg where
  x == y = show x == show y

instance Ord DisplayMsg where
  compare x y = compare (show x) (show y)

type RenderState = State (M.Map DisplayMsg Int)

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
                      Symbol -> PPT.italicized
                      Success -> PPT.colorDull PPT.Green
                      Failure -> PPT.bold <> PPT.colorDull PPT.Red

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

renderAsTree :: MatchTree -> Message
renderAsTree t =
  if M.null refs
  then doc
  else doc <> hardline <> "where:" <> hardline <> indent 2 (renderRefs refs)
  where
    (doc, refs) = runState (renderAsTreeWithRefs t) M.empty
    swap (x, y) = (y, x)
    renderRefs = vsep . map renderRef . IM.toList . IM.fromList . map swap . M.toList
    renderRef (id, val) = displayRef id <+> unDisplay val

allocateId :: Message -> RenderState Int
allocateId str = do
  m <- get
  let msg = DisplayMsg str
  case M.lookup msg m of
    Just i -> return i
    Nothing -> do
      let n = 1 + M.size m
      put $ M.insert msg n m
      return n

renderAsTreeWithRefs :: MatchTree -> RenderState Message
renderAsTreeWithRefs (MatchTree res descr val subnodes) = do
  doc <- lineDoc
  subtreeDocs <- traverse renderAsTreeWithRefs subnodes
  let subtreeDoc = indent 2 (vsep subtreeDocs)
  let prefix = annotate msgStyle (if res then check else cross)
  return $  prefix <+> if null subnodes then doc else vsep [doc, subtreeDoc]
  where msgStyle = if res then Success else Failure
        aDescr = annotate msgStyle descr
        limit = 20
        lineDoc = case val of
          Nothing -> return aDescr
          Just valMsg | length (show valMsg) > limit -> do
                        id <- allocateId valMsg
                        return $ hsep [aDescr, arrow, displayRef id]
          Just valMsg -> return $ hsep [aDescr, arrow, valMsg]

check, cross, arrow :: Message
check = "✔"
cross = "✘"
arrow = "←"

displayRef :: Int -> Message
displayRef ref = hcat ["<", pretty ref, ">"]

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
