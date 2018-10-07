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

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
{- |
Module:       Test.Matchers.Simple
Description:  Simple matcher combinators for better unit testing.
Copyright:    2018 Google LLC
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

This module provides simple matcher combinators to simplify
unit-testing and make tests easier to debug.

-}
module Test.Matchers.Simple
  ( MatcherF
  , Matcher
  , MatchTree(..)

  , simpleMatcher
  , aggregateMatcher

  -- * Matchers for 'Eq' and 'Ord' types
  , eq
  , lt
  , le
  , gt
  , ge

  -- * Matchers combinators
  , MatcherSetF
  , matcher
  , matchers
  , anything
  , allOf
  , oneOf
  , inverseOf
  , andAlso
  , orElse

  -- * Matchers for sequences
  , isEmpty
  , lengthIs
  , elementsAre
  , startsWith
  , endsWith
  , hasInfix

  -- * Matchers for basic types from the standard library
  , tuple2
  , leftIs
  , rightIs
  , property
  , prism
  , contramap

  -- * Exception matching
  , throws

  -- * Executing matchers
  , match

  -- * Operators
  , (&.)
  , (&>)
  ) where

import           Test.Matchers.Message

import           Control.Applicative   (liftA2)
import           Control.Exception     (Exception (..), Handler (..),
                                        SomeException, catches)
import           Data.Foldable         (Foldable, foldMap, null, toList)
import           Data.Functor.Identity (Identity (..), runIdentity)
import           Data.List             (isInfixOf, isPrefixOf, isSuffixOf)
import           Data.Traversable      (Traversable)
import           Data.Typeable         (typeOf)

-- | Matcher is a function from optional values to match trees.
-- 'Nothing' means that there's no value to match. This is not useful
-- directly but allows the library to construct meaningful messages
-- when some parts of the structure being matched are missing.  The
-- @f@ parameter is there so that we can match not just pure values
-- but IO actions as well, see the 'throws' combinator as an example.
type MatcherF f a = Maybe (f a) -> f MatchTree

-- | A (possibly empty) group of matchers. A set of matchers can be
-- turned into a proper matcher via aggregation functions,
-- e.g. 'allOf' or 'oneOf'.
--
-- Matcher sets can be composed \"sequentially\" or in
-- \"parallel\".
--
-- \"Parallel\" composition combines @MatcherSetF f a@ and
-- @MatcherSetF f b@ into @MatcherSetF f (a, b)@ and is achieved via the
-- '&>' operator.
--
-- @
--           ╭───────── &. ─────────┬───────────────────────╮
--           ┴                      ┴                       ╽
--           a                      b                     (a,b)
--           ┬                      ┬                       ┬
--           ╽                      ╽                       ╽
--  ╭─────────────────╮    ╭─────────────────╮   ╭─────────────────────╮
--  │ MatcherSetF f a │ &> │ MatcherSetF f a │ = │ MatcherSetF f (a,b) │
--  ╰─────────────────╯    ╰─────────────────╯   ╰─────────────────────╯
-- @
--
-- \"Sequential\" composition combines multiple @MatcherSetF f a@ into one
-- and is achieved via 'mappend'.
--
-- @
--                       a
--                       ┬
--  ╭─────────────────╮  │
--  │ MatcherSetF f a │ ╾┤
--  ╰─────────────────╯  │
--        mappend        │
--  ╭─────────────────╮  │
--  │ MatcherSetF f a │ ╾┤
--  ╰─────────────────╯  │
--           =           │
--  ╭─────────────────╮  │
--  │ MatcherSetF f a │ ╾╯
--  ╰─────────────────╯
-- @
newtype MatcherSetF f a = MatcherSetF { matchF :: Maybe (f a) -> f [MatchTree] }

-- | A specialization of the 'MatcherF' that can only match pure
-- values.
type Matcher a    = MatcherF Identity a

-- | Should the fancy colors be used when pretty-printing mismatches.
data ColorMode = Color | NoColor

-- | The result of a matcher invokation.
data MatchTree
  = MatchTree
    { mtValue          :: !Bool -- ^ Whether the match was successful.
    , mtDescription    :: Message -- ^ Message describing of the matcher.
    , mtInvDescription :: Message -- ^ Message describing the inversion of the matcher.
    , mtMatchedValue   :: Message -- ^ Textual representation of the value matched.
    , mtSubnodes       :: [MatchTree] -- ^ Submatchers used to produce this result.
    } deriving (Show)

instance Eq MatchTree where
  (MatchTree outcome descr descr_ val subs) == (MatchTree outcome' descr' descr_' val' subs') =
    outcome == outcome'
    && show descr == show descr'
    && show descr_ == show descr_'
    && show val == show val'
    && subs == subs'

instance (Applicative f) => Monoid (MatcherSetF f a) where
  mempty = MatcherSetF $ const $ pure []
  mappend (MatcherSetF l) (MatcherSetF r) = MatcherSetF $ \x -> liftA2 mappend (l x) (r x)

noValueMessage :: Message
noValueMessage = "nothing"

inputToDocF :: (Applicative f, Show a) => Maybe (f a) -> f Message
inputToDocF Nothing   = pure noValueMessage
inputToDocF (Just fa) = display <$> fa

inputToDoc :: (Show a) => Maybe a -> Message
inputToDoc = maybe noValueMessage display

-- | Makes a matcher from a predicate and it's description.
simpleMatcher :: (Show a, Applicative f)
              => (a -> Bool) -- ^ Predicate to use for matching
              -> Message -- ^ Message describing this predicate.
              -> Message -- ^ Message describing the inversion of the predicate.
              -> MatcherF f a
simpleMatcher predicate descr invDescr v =
  case v of
    Nothing -> pure $ MatchTree False descr invDescr noValueMessage []
    Just fa -> (\x -> MatchTree (predicate x) descr invDescr (inputToDoc (Just x)) []) <$> fa


aggregateWith :: (Show a, Applicative f)
              => ([Bool] -> Bool)
              -> Message
              -> Message
              -> MatcherSetF f a
              -> MatcherF f a
aggregateWith aggr descr invDescr matcherSet value =
  let subnodesF = matchF matcherSet value
      msgF = inputToDocF value
  in liftA2 (\xs m -> MatchTree (aggr $ map mtValue xs) descr invDescr m xs) subnodesF msgF

-- | Makes a matcher that aggregates results of submatchers.
aggregateMatcher :: (Show a, Applicative f)
                 => ([Bool] -> Bool) -- ^ Subnode result aggregation function.
                 -> Message -- ^ Message describing success of this matcher.
                 -> Message -- ^ Message describing failure of this matcher.
                 -> [MatcherF f a]   -- ^ Matchers to aggregate.
                 -> MatcherF f a
aggregateMatcher aggF descr invDescr ms = doMatch
  where
    doMatch x = mkAgg <$> inputToDocF x <*> sequenceA (map ($ x) ms)
    mkAgg val nodes = MatchTree (aggF $ map mtValue nodes) descr invDescr val nodes

-- | Matcher that succeeds if the argument equals to the specified
-- value.
eq :: (Eq a, Show a, Applicative f)
   => a          -- ^ The value that the argument must be equal to
   -> MatcherF f a
eq value = simpleMatcher (== value) descr descr_
  where descr  = hsep ["a", "value", "equal", "to", display value]
        descr_ = hsep ["a", "value", "not", "equal", "to", display value]

-- | Mathcer that succeeds if the argument is /greater than/ the
-- specified value.
gt :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
gt = cmpSatisfies (== GT) ">" "≤"

-- | Mathcer that succeeds if the argument is /greater than or equal to/
-- the specified value.
ge :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
ge = inverseOf . lt

-- | Mathcer that succeeds if the argument is /less than/ the
-- specified value.
lt :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
lt = cmpSatisfies (== LT) "<" "≥"

-- | Mathcer that succeeds if the argument is /less than or equal to/
-- the specified value.
le :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
le = inverseOf . gt

-- | A helper function to contruct matcher for 'Ord' types
cmpSatisfies
  :: (Ord a, Show a, Applicative f)
  => (Ordering -> Bool) -- ^ Predicate matching the outcome of 'compare'.
  -> String -- ^ Comparison symbol describing the matcher.
  -> String -- ^ Comparison symbol describing the inverse of the matcher.
  -> a -- ^ Value to compare against.
  -> MatcherF f a
cmpSatisfies p symbol symbol_ bound = simpleMatcher (\x -> p $ compare x bound) descr descr_
  where descr  = hsep ["a", "value", pretty symbol,  display bound]
        descr_ = hsep ["a", "value", pretty symbol_, display bound]

-- | Matcher that always succeeds.
anything :: (Show a, Applicative f) => MatcherF f a
anything = simpleMatcher (const True) "anything" "nothing"

-- | Constructs a matcher set from a 'Foldable' container containing
-- matchers.
matchers
  :: (Applicative f, Foldable t)
  => t (MatcherF f a)
  -> MatcherSetF f a
matchers = foldMap matcher

-- | Constructs a matcher set containing just a single matcher.
matcher
  :: (Functor f)
  => MatcherF f a
  -> MatcherSetF f a
matcher = MatcherSetF . fmap (fmap pure)

-- | Constructs a matcher that succeed if all the matchers in the
-- provided list succeed.
allOf :: (Show a, Applicative f) => MatcherSetF f a -> MatcherF f a
allOf = aggregateWith and "all of" "not all of"

-- | Constructs a matcher that succeed if at least one of the matchers
-- in the provided list succeed.
oneOf :: (Show a, Applicative f) => MatcherSetF f a -> MatcherF f a
oneOf = aggregateWith or "one of" "none of"

-- | Inverts the given matcher.
--
-- prop> forall m. inverseOf (inverseOf m) == m
-- prop> forall x m. x matches m ⇒ not (x matches (inverseOf m))
inverseOf :: (Show a, Applicative f)
           => MatcherF f a -- ^ The matcher to inverse.
           -> MatcherF f a
inverseOf = fmap (fmap inv)
  where inv (MatchTree res descr descr_ val subnodes) =
          MatchTree (not res) descr_ descr val subnodes

-- | A version of 'allOf' specialized for two submatchers.
andAlso :: (Show a, Applicative f) => MatcherF f a -> MatcherF f a -> MatcherF f a
andAlso l r = allOf $ matchers [l, r]

-- | A version of 'oneOf' specialized for two submatchers.
orElse :: (Show a, Applicative f) => MatcherF f a -> MatcherF f a -> MatcherF f a
orElse l r = oneOf $ matchers [l, r]

-- | Checks that the container has no values.
isEmpty :: (Show (t a), Foldable t, Applicative f) => MatcherF f (t a)
isEmpty = simpleMatcher null "is empty" "is not empty"

-- | Checks that container length satisfies the given matcher.
lengthIs
  :: (Show (t a), Foldable t, Applicative f)
  => MatcherF f Int -- ^ Matcher for the container size.
  -> MatcherF f (t a)
lengthIs = property "length" length

-- | Checks that elements of the container are matched by the matchers
-- exactly. The number of elements in the container should match the
-- number of matchers in the list.
elementsAre :: (Foldable t, Monad f, Show a, Show (t a))
            => [MatcherF f a] -- ^ List of matchers for the elements of the list.
            -> MatcherF f (t a) -- ^ Matcher for the whole container.
elementsAre matchers = maybe emptyTree mkTree
  where
    emptyTree = MatchTree False name name_ noValueMessage <$> sequenceA (map ($ Nothing) matchers)

    mkTree fitems = do
      items <- fitems
      subnodes <- sequenceA $ go (toList items) matchers 0
      return $ MatchTree (all mtValue subnodes) name name_ (display items) subnodes

    numMatchers = length matchers
    name  = hsep ["has", display numMatchers, elems numMatchers]
    name_ = hsep ["does", "not", "have", display numMatchers, elems numMatchers]
    elems n = if n == 1 then "element" else "elements"

    sizeMessage  = hsep ["number", "of", "elements", "is", display numMatchers]
    sizeMessage_ = hsep ["number", "of", "elements", "is", "not", display numMatchers]

    go (x:xs) (m:ms) n = m (Just $ pure x) : go xs ms (n + 1)
    go [] [] n = [pure $ MatchTree True sizeMessage sizeMessage_ (display n) []]
    go moreItems@(x:_) [] n = [pure $ MatchTree False sizeMessage sizeMessage_
                                (display $ n + length moreItems) []]
    go [] ms@(m:_) n = [ m Nothing
                       , pure $ MatchTree False sizeMessage sizeMessage_
                         (display n) []
                       ]

-- | Matcher that succeeds if the argument starts with the specified
-- prefix.
startsWith :: (Applicative f, Show a, Eq a)
           => [a]            -- ^ The prefix the list is expected to have.
           -> MatcherF f [a]
startsWith xs = simpleMatcher (xs `isPrefixOf`)
                (hsep ["starts", "with", display xs])
                (hsep ["does", "not", "start", "with", display xs])

-- | Matcher that succeeds if the argument ends with the specified
-- suffix.
endsWith :: (Applicative f, Show a, Eq a)
           => [a]            -- ^ The suffix the list is expected to have.
           -> MatcherF f [a]
endsWith xs = simpleMatcher (xs `isSuffixOf`)
              (hsep ["ends", "with", display xs])
              (hsep ["does", "not", "end", "with", display xs])

-- | Matcher that succeeds if the argument contains the given infix.
hasInfix :: (Applicative f, Show a, Eq a)
           => [a]            -- ^ The infix the list is expected to have.
           -> MatcherF f [a]
hasInfix xs = simpleMatcher (xs `isInfixOf`)
              (hsep ["has", "infix", display xs])
              (hsep ["does", "not", "have", "infix", display xs])

-- | Builds a matcher for a pair from the matchers of components.
tuple2
  :: (Show a, Show b, Applicative f)
  => MatcherF f a -- ^ Matcher for the 1st element of the pair.
  -> MatcherF f b -- ^ Matcher for the 2nd element of the pair.
  -> MatcherF f (a, b)
tuple2 mx my = property "fst" fst mx `andAlso` property "snd" snd my

-- | Makes a matcher that only matches 'Left' values satisfying given
-- matcher.
leftIs
  :: (Show a, Show b, Traversable f, Applicative f)
  => MatcherF f a -- ^ Matcher for the left side of 'Either'.
  -> MatcherF f (Either a b)
leftIs  = prism "Left"  $ \x -> case x of { Left a  -> Just a; _ -> Nothing }

-- | Makes a matcher that only matches 'Right' values satisfying given
-- matcher.
rightIs
  :: (Show a, Show b, Traversable f, Applicative f)
  => MatcherF f b -- ^ Matcher for the right side of 'Either'.
  -> MatcherF f (Either a b)
rightIs = prism "Right" $ \x -> case x of { Right b -> Just b; _ -> Nothing }

-- | Implementation of Data.Functor.Covariant.contramap.
-- Avoid using it directly, prefer 'property' instead.
contramap :: (Functor f) => (s -> a) -> MatcherF f a -> MatcherF f s
contramap f p = p . fmap (fmap f)

-- | Builds a matcher for a structure from a matcher of its substructure.
--
-- It's equivalent to 'contramap' but also takes a name for
-- mismatch reporting.
property :: (Show a, Show s, Applicative f)
         => String       -- ^ The name of the property of the structure 's'
         -> (s -> a)     -- ^ The projection from a structure 's' to it's substructure 'a'.
         -> MatcherF f a -- ^ Matcher of the substructure 'a'.
         -> MatcherF f s
property name proj m = aggregateMatcher and descr descr_ [contramap proj m]
  where descr  = hsep ["property", pretty name, "is"]
        descr_ = hsep ["property", pretty name, "is", "not"]

-- | Builds a matcher for one alternative of a sum type given matcher for a
-- prism projection.
prism :: (Show s, Show a, Traversable f, Applicative f)
      => String         -- ^ The name of the selected alternative of the structure 's'.
      -> (s -> Maybe a) -- ^ The projection that tries to select the alternative 'a'.
      -> MatcherF f a   -- ^ The matcher for the value of the alternative.
      -> MatcherF f s
prism name p m v =
  case v of
    Nothing -> (MatchTree False descr descr_ noValueMessage . pure) <$> m Nothing
    Just fs ->
      (\n s -> MatchTree (mtValue n) descr descr_ (display s) [n])
      <$> m (sequenceA $ fmap p fs)
      <*> fs
  where
    descr  = hsep ["prism", pretty name, "is"]
    descr_ = hsep ["prism", pretty name, "is", "not"]

-- | Represents a result of IO action execution.
data ActionOutcome e
  = NoExn                  -- ^ The action completed successfully.
  | ExpectedExn e          -- ^ The action threw an exception of the expected type.
  | OtherExn SomeException -- ^ The action threw an unexpected exception.

tryExn :: forall a e. (Exception e)
       => IO a
       -> IO (ActionOutcome e)
tryExn ma =
  fmap (const NoExn) ma `catches`
  [ Handler (\(exn :: e) -> pure $ ExpectedExn exn)
  , Handler (\(exn :: SomeException) -> pure $ OtherExn exn)
  ]

-- | Checks that an IO action throws an exception satisfying the
-- given matcher.
throws :: forall e a. (Exception e)
       => Matcher e  -- ^ Matcher for the exception value.
       -> MatcherF IO a
throws exMatcher maybeAction = do
  let excName = display $ typeOf (undefined :: e)
      descr  = hsep ["action", "throwing", excName, "that", "is"]
      descr_  = hsep ["action", "not", "throwing", excName, "that", "is"]
      
  case maybeAction of
    Nothing ->
      return $ MatchTree False descr descr_ noValueMessage [runIdentity $ exMatcher Nothing]
    Just action -> do
      outcome <- tryExn action
      case outcome of
        NoExn -> return $ MatchTree False descr descr_ noValueMessage [runIdentity $ exMatcher Nothing]
        ExpectedExn exn -> return $ let node = match exn exMatcher
                                    in MatchTree (mtValue node) descr descr_ (display exn) [node]
        OtherExn exn -> return $ MatchTree False descr descr_ (display exn) [runIdentity $ exMatcher Nothing]

-- | Runs a pure matcher on the given value and returns the
-- 'MatchTree'.
match
  :: (Show a)
  => a -- ^ Value to match.
  -> Matcher a -- ^ Matcher to run on the value.
  -> MatchTree
match x p = runIdentity $ p (Just $ Identity x)

-- | Returns 'True' if the given value matches the provided pure
-- matcher, otherwise returns 'False'.
matches
  :: (Show a)
  => a -- ^ Value to match.
  -> Matcher a -- ^ Matcher to check
  -> Bool
matches x m = mtValue $ match x m

-- Combinators

-- | Helper operator to make the construction of nested pairs look a
-- bit nicer.  Mostly useful for defining custom matchers.
--
-- >>> 1 &. 2
-- (1,2)
--
-- >>> 1 &. 2 &. 3
-- (1,(2,3))
--
-- >>> 1 &. 2 &. 3 &. 4
-- (1,(2,(3,4)))
(&.) :: a -> b -> (a, b)
x &. y = (x, y)

infixl 7 &.

-- | Parallel matcher set composition operator, see 'MatcherSetF' for details.
(&>) :: (Show a, Show b, Applicative f)
     => MatcherSetF f a
     -> MatcherSetF f b
     -> MatcherSetF f (a, b)
ma &> mb = MatcherSetF $ \maybeP ->
  liftA2 mappend
  (matchF ma $ fmap (fmap fst) maybeP)
  (matchF mb $ fmap (fmap snd) maybeP)

infixl 7 &>
