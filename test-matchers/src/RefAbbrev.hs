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
module RefAbbrev where

import Test.Matchers.LexShow (Token)

import Control.Arrow (second, (***), (&&&))

import Data.Tuple (swap)
import Data.Function (fix, on)
import Data.Maybe (mapMaybe)
import Data.List (mapAccumL, groupBy, sortBy)

newtype Automaton i o
  = Automaton { delta :: i -> (o, Automaton i o) }

instance (Monoid o) => Monoid (Automaton i o) where
  mempty = Automaton $ \_ -> (mempty, mempty)
  m1 `mappend` m2 = Automaton $ \i -> delta m1 i `mappend` delta m2 i

instance Functor (Automaton i) where
  fmap f m = Automaton $ \i -> f *** (fmap f) $ delta m i

instance Applicative (Automaton i) where
  pure x =  fix (\self -> Automaton $ const (x, self))
  mf <*> ma = Automaton $ \x -> let (f, mf') = delta mf x
                                    (a, ma') = delta ma x
                                in (f a, mf' <*> ma')

stepAutomaton :: Automaton i o -> i -> Automaton i o
stepAutomaton m = snd . delta m

groupH :: (Ord a) => [([a], b)] -> [(a, [([a], b)])]
groupH =
  map ((fst . head) &&& map snd)
  . groupBy ((==) `on` fst)
  . sortBy (compare `on` fst)
  . mapMaybe (\(xs, i) -> fmap (second $ flip (,) i) $ uncons xs)
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
          mkSubAutomaton (c, nb) = (c, (match nb, build nb $ stepAutomaton onfail c))
          successes = map mkSubAutomaton groups
          match =  map snd . filter (null . fst)
      in Automaton $ \c -> case lookup c successes of
                             Nothing -> (mempty, stepAutomaton onfail c)
                             Just sc -> sc

runAutomaton :: Automaton Token [Int] -> [Token] -> [[Int]]
runAutomaton m = snd . mapAccumL step m
  where step m tok = swap $ delta m tok
  
-- | Compresses strings by replacing values with references.
--
-- Example:
-- @
-- abbreviate $ fromList
--              [ ("User {id=5, name=\"John Smith\"}", 1
--              , ("\"John Smith\"", 2)
--              , ("Right (User {id=5, name=\"John Smith\"}), 3"
--              ]
-- @
-- Should produce
-- @
-- fromList $ [ ("User {id=5, name=<2>}", 1]
--            , ("\"John Smith\"", 2)
--            , (Right (<1>), 3)
-- @
--
-- Note that this function assumes that the strings are haskell
-- objects printed with 'show'.
abbreviate :: [([Token], Int)] -> [([Token], Int)]
abbreviate _refs' = undefined
