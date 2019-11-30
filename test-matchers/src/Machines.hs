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
{-
|
Module:       Machines
Description:  Lexer combinators library.
Copyright:    2018-2019 Google LLC
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

This module contains combinators for definining lexers that never
backtrack.
-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Machines where

import Control.Applicative (Alternative(empty, (<|>)))
import Control.Arrow (first, (&&&))
import Control.Category ((>>>))
import Data.Char (isSpace, toUpper)
import Data.Functor (($>))
import Data.List (groupBy, partition, sortBy)

import Data.Function (on)
import qualified Data.Text as T

data Input
  = Val !Int !Char
  | Eof !Int

position :: Input -> Int
position (Eof n) = n
position (Val n _) = n

data State a
  = Done a
  | Cont (Machine a)
  | Failed

instance (Show a) => Show (State a) where
  show (Done x) = "Done " ++ show x
  show Failed = "Failed"
  show (Cont _) = "Cont <fun>"

newtype Machine a = Machine { step :: Input -> State a }

instance Functor State where
  fmap f (Done x) = Done (f x)
  fmap f (Cont c) = Cont (fmap f c)
  fmap _ Failed = Failed

instance Applicative State where
  pure = Done
  (Done f) <*> (Done x) = Done (f x)
  (Done f) <*> (Cont c) = Cont (f <$> c)
  (Cont f) <*> (Done x) = Cont (fmap ($ x) f)
  (Cont f) <*> (Cont c) = Cont (f <*> c)
  _ <*> Failed = Failed
  Failed <*> _ = Failed

instance Monad State where
  return = pure
  (Done x) >>= f = f x
  (Cont c) >>= f = Cont (Machine $ (>>= f) <$> step c)
  Failed   >>= _ = Failed
  fail = const Failed

instance Functor Machine where
  fmap f (Machine x) = Machine $ fmap (fmap f) x

instance Applicative Machine where
  pure = Machine . const . Done
  f <*> x = Machine $ \i -> case step f i of
                              Done f' -> f' <$> step x i
                              Cont f' -> Cont (f' <*> x)
                              Failed -> Failed

instance Alternative Machine where
  empty = Machine (const Failed)
  f <|> g = Machine $ \i -> case step f i of
                              done@(Done _) -> case step g i of
                                                 Failed -> done
                                                 -- Prefer the first match
                                                 (Done _) -> done
                                                 -- Prefer the longest match
                                                 cont@(Cont _) -> cont
                              Failed -> step g i
                              cf@(Cont f') -> case step g i of
                                                Done _ -> cf
                                                Failed -> cf
                                                Cont g' -> Cont (f' <|> g')

instance Monad Machine where
  return = pure
  m >>= f = Machine $ \i -> case step m i of
                              Done x -> step (f x) i
                              Cont c -> Cont (c >>= f)
                              Failed -> Failed

instance (Semigroup a) => Semigroup (Machine a) where
  m1 <> m2 = Machine $ \i -> case step m1 i of
                               Failed -> Failed
                               Done x -> fmap ((<>) x) (step m2 i)
                               Cont c -> Cont (c <> m2)

instance (Monoid a) => Monoid (Machine a) where
  mempty = pure mempty

----------------------------------------------------------------------
-- Ranges
----------------------------------------------------------------------

data Range = Range !Int !Int deriving (Show, Ord, Eq)

isEmptyR :: Range -> Bool
isEmptyR (Range b e) = b >= e

singleton :: Int -> Range
singleton n = Range n (n + 1)

instance Semigroup Range where
  l@(Range b1 e1) <> r@(Range b2 e2)
    | isEmptyR l = r
    | isEmptyR r = l
    | otherwise = Range (min b1 b2) (max e1 e2)

instance Monoid Range where
  mempty = Range 0 0

(|+) :: Range -> Int -> Range
r |+ n = r `mappend` singleton n

----------------------------------------------------------------------
-- Generic machine combinators
----------------------------------------------------------------------

opt :: (Monoid a) => Machine a -> Machine a
opt m = Machine $ \i -> case step m i of
                          Failed -> Done mempty
                          other -> other

foldrMany :: (a -> b -> b) -> b -> Machine a -> Machine b
foldrMany f b m = Machine $ \i -> case i of
                                    Eof _ -> Done b
                                    (Val _ _) -> case step m i of
                                                   Failed -> Done b
                                                   Done x -> Done $ f x b
                                                   Cont next -> Cont (f <$> next <*> foldrMany f b m)

many :: Machine a -> Machine [a]
many = foldrMany (:) []

many1 :: Machine a -> Machine [a]
many1 m = (:) <$> m <*> many m

foldMany :: (Monoid a) => Machine a -> Machine a
foldMany = foldrMany mappend mempty

foldMany1 :: (Monoid a) => Machine a -> Machine a
foldMany1 m = mappend <$> m <*> foldMany m

scan :: s -> (s -> Int -> Char -> Maybe s) -> Machine s
scan s next = Machine $ \case
                           Eof _ -> Done s
                           Val n c -> case next s n c of
                                        Nothing -> Done s
                                        Just s' -> Cont (scan s' next)

guard :: (Char -> Bool) -> Machine ()
guard p = Machine $ \case
                      Eof _ -> Failed
                      Val _ c -> if p c then Done () else Failed

skipSpaces :: Machine ()
skipSpaces = foldMany (satisfying isSpace) $> ()

----------------------------------------------------------------------
-- Range machines
----------------------------------------------------------------------

satisfying :: (Char -> Bool) -> Machine Range
satisfying p = Machine $ \case
                            Eof _ -> Failed
                            Val n c -> if p c
                                       then Cont $ pure $ Range n (n + 1)
                                       else Failed

getRange :: Machine Range
getRange = Machine $ \i -> let n = position i in Done $ Range n n

char :: Char -> Machine Range
char c = satisfying (== c)

charCase :: Char -> Machine Range
charCase c = satisfying (((==) `on` toUpper) c)

oneOfChars :: String -> Machine Range
oneOfChars s = satisfying (`elem` s)

charRange :: Char -> Char -> Machine Range
charRange from to = satisfying (\c -> from <= c && c <= to)

str :: String -> Machine Range
str = go mempty
  where go !r [] = fmap (mappend r) getRange
        go !r (x:xs) = Machine $ \case
                                    Eof _ -> Failed
                                    Val n c -> if c == x
                                               then Cont $ go (r |+ n) xs
                                               else Failed

newtype CharTrie = CharTrie [(Maybe Char, CharTrie)]
  deriving (Show, Eq)

buildTrie :: [String] -> CharTrie
buildTrie xs = CharTrie $ map (fmap buildTrie) classes
  where (emptyStr, nonEmpty) = partition null xs
        bucket :: (Ord key) => (a -> (key, val)) -> [a] -> [(key, [val])]
        bucket by = map by
                    >>> sortBy (compare `on` fst)
                    >>> groupBy ((==) `on` fst)
                    >>> map (\entries@((key, _):_) -> (key, map snd entries))
        classes :: [(Maybe Char, [String])]
        classes = let bs = map (first Just) $ bucket (head &&& tail) nonEmpty
                  in if null emptyStr
                     then bs
                     else (Nothing, []):bs

-- | oneOfStr [x₁,x₂, …] is a slightly more efficient equivalent of
-- str x₁ <|> str x₂ <|> …
--
-- The key difference is that it builds a dictionary in the form of
-- trie in advance and uses it instead of holding many machines in parallel.
-- On the list of C++ keywords, it's ~3 times faster.
oneOfStr :: [String] -> Machine Range
oneOfStr = go mempty . buildTrie
  where go !r (CharTrie []) = fmap (mappend r) getRange
        go !r (CharTrie xs) = Machine (match r xs)

        match !r t (Eof n) = case lookup Nothing t of
                               Nothing -> Failed
                               Just _ -> Done (r |+ n)
        match !r t (Val n c) = case lookup (Just c) t of
                                 Just node -> Cont $ go (r |+ n) node
                                 Nothing -> case lookup Nothing t of
                                              Just _ -> Done r
                                              Nothing -> Failed

----------------------------------------------------------------------
-- Running the machines
----------------------------------------------------------------------

runMachineText :: Machine a -> T.Text -> (Int, State a, T.Text)
runMachineText machine = go machine 0
  where go m n s = case T.uncons s of
                     Just (x, xs) -> case step m (Val n x) of
                                       Cont next -> go next (n + 1) xs
                                       other -> (n, other, s)
                     Nothing -> (n, step m (Eof n), T.empty)

runMachine :: Machine a -> String -> (State a, String)
runMachine machine = go machine 0
  where go m n "" = (step m (Eof n), "")
        go m n s@(x:xs) = case step m (Val n x) of
                            Cont next -> go next (n + 1) xs
                            other -> (other, s)
