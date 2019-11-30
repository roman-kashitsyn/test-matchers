{-
Copyright 2018-2019 Google LLC

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

{-# LANGUAGE TupleSections #-}
module RefAbbrev (abbreviate, abbreviateResult) where

import Test.Matchers.LexShow
  ( LexResult
  , Token
  , mapLexResult
  , refToToken
  , resultTokens
  )

import Control.Arrow (first, second, (&&&), (***))

import qualified Data.IntMap as IM

import Data.Function (on)
import Data.List (groupBy, mapAccumL, maximumBy, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Tuple (swap)

type RefId = Int
type RefsLengths = IM.IntMap Int

newtype Automaton i o
  = Automaton { delta :: i -> (o, Automaton i o) }

stepAutomaton :: Automaton i o -> i -> Automaton i o
stepAutomaton m = snd . delta m

outputOn :: Automaton i o -> i -> o
outputOn m = fst . delta m

groupH :: (Ord a) => [([a], b)] -> [(a, [([a], b)])]
groupH =
  map ((fst . head) &&& map snd)
  . groupBy ((==) `on` fst)
  . sortBy (compare `on` fst)
  . mapMaybe (\(xs, i) -> second (, i) <$> uncons xs)
  where
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)

-- Aho-Corasick
mkAutomaton :: [([Token], Int)] -> Automaton Token [Int]
mkAutomaton needles = top
  where
    top = let m = build needles (Automaton $ const ([], m))
          in m

    build ns onfail =
      let groups = groupH ns
          mkSubAutomaton (c, nb) = (c, ( match nb `mappend` outputOn onfail c
                                       , build nb $ stepAutomaton onfail c
                                       )
                                   )
          successes = map mkSubAutomaton groups
          match =  map snd . filter (null . fst)
      in Automaton $ \c -> fromMaybe (delta onfail c) (lookup c successes)

runAutomaton :: Automaton Token [RefId] -> [Token] -> [[RefId]]
runAutomaton m = snd . mapAccumL step m
  where step a tok = swap $ delta a tok


shorten :: RefsLengths -> [(Token, [RefId])] -> [Either RefId Token]
shorten lengths = go []
  where go outs ((tok, []):rest) = go (Right tok : outs) rest
        go outs ((tok, refs):rest) = let outs' = Right tok : outs
                                     in go (fromMaybe outs' $ tryPopRef (longestRef refs) outs') rest
        go outs [] = reverse outs

        longestRef :: [RefId] -> (RefId, Int)
        longestRef refs = let ref = maximumBy (compare `on` (lengths IM.!)) refs
                          in (ref, lengths IM.! ref)

        tryPopRef (ref, 0) outs = Just $ Left ref : outs
        tryPopRef (_,   _) [] = Nothing
        tryPopRef (ref, len) (Left ref' : rest) = let len' = lengths IM.! ref'
                                                  in if len >= len'
                                                     then tryPopRef (ref, len - len') rest
                                                     else Nothing
        tryPopRef (ref, len) (Right _ : rest) = tryPopRef (ref, len - 1) rest

makeAbbreviator :: [([Token], RefId)] -> ([Token], RefId) -> ([Token], RefId)
makeAbbreviator allRefs =
  let automaton = mkAutomaton allRefs
      lengths = IM.fromList $ map (swap . first length) allRefs
      annotate ref toks = zip toks $ map (filter (/= ref)) $ runAutomaton automaton toks
      abbrev (toks, ref) = (map (either refToToken id) $ shorten lengths $ annotate ref toks, ref)
  in abbrev


-- | Compresses token lists by replacing matching subsequences with
-- the corresponding references.
abbreviate :: [([Token], RefId)] -> [([Token], RefId)]
abbreviate refs = map (makeAbbreviator refs) refs

abbreviateResult :: [(LexResult, RefId)] -> [(LexResult, RefId)]
abbreviateResult refs =
  let abbrev = makeAbbreviator $ map (first resultTokens) refs
      applyToToks :: (([Token], RefId) -> ([Token], RefId))
                  -> (LexResult, RefId) -> (LexResult, RefId)
      applyToToks toksEndo (res, refId) = (mapLexResult (\toks -> fst $ toksEndo (toks, refId)) res, refId)
  in map (applyToToks abbrev) refs

