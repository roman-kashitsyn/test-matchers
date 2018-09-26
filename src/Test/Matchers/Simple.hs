{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_HADDOCK hide #-}
{- |
Module:       Test.Matchers.Simple
Description:  Simple matcher combinators for better unit testing.
Copyright:    (c) Roman Kashitsyn, 2018
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
  , doNotMatch
  , andAlso
  , orElse

  -- * Matchers for sequences
  , isEmpty
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
  = Node
    { nodeValue        :: !Bool       -- ^ Whether the match was successful.
    , nodeMessage      :: Message     -- ^ Message describing this matcher.
    , nodeMatchedValue :: Message     -- ^ String representation of the value matched.
    , nodeSubnodes     :: [MatchTree] -- ^ Submatchers used to produce this result.
    } deriving (Show)

instance Eq MatchTree where
  (Node outcome msg val subs) == (Node outcome' msg' val' subs') =
    outcome == outcome'
    && (show msg) == (show msg')
    && (show val == show val')
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
              -> Message     -- ^ Message describing the predicate
              -> MatcherF f a
simpleMatcher predicate msg v =
  case v of
    Nothing -> pure $ Node False msg noValueMessage []
    Just fa -> (\x -> Node (predicate x) msg (inputToDoc (Just x)) []) <$> fa


aggregateWith :: (Show a, Applicative f)
              => ([Bool] -> Bool)
              -> Message
              -> MatcherSetF f a
              -> MatcherF f a
aggregateWith aggr description matcherSet value =
  let subnodesF = matchF matcherSet value
      msgF = inputToDocF value
  in liftA2 (\xs m -> Node (aggr $ map nodeValue xs) description m xs) subnodesF msgF

-- | Makes a matcher that aggregates results of submatchers.
aggregateMatcher :: (Show a, Applicative f)
                 => ([Bool] -> Bool) -- ^ Subnode result aggregation function
                 -> Message          -- ^ Message describing this matcher
                 -> [MatcherF f a]   -- ^ Matchers to aggregate
                 -> MatcherF f a
aggregateMatcher aggF msg ms = doMatch
  where
    doMatch x = mkAgg <$> inputToDocF x <*> sequenceA (map ($ x) ms)
    mkAgg val nodes = Node (aggF $ map nodeValue nodes) msg val nodes

-- | Matcher that succeeds if the argument equals to the specified
-- value.
eq :: (Eq a, Show a, Applicative f)
   => a          -- ^ The value that the argument must be equal to
   -> MatcherF f a
eq value = simpleMatcher (== value) message
  where message = hsep ["a", "value", "equal", "to", display value]

-- | Mathcer that succeeds if the argument is /greater than/ the
-- specified value.
gt :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
gt = cmpSatisfies (== GT) ">"

-- | Mathcer that succeeds if the argument is /greater than or equal to/
-- the specified value.
ge :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
ge = cmpSatisfies ((||) <$> (== GT) <*> (== EQ)) "≥"

-- | Mathcer that succeeds if the argument is /less than/ the
-- specified value.
lt :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
lt = cmpSatisfies (== LT) "<"

-- | Mathcer that succeeds if the argument is /less than or equal to/
-- the specified value.
le :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
le = cmpSatisfies ((||) <$> (== LT) <*> (== EQ)) "≤"

-- | A helper function to contruct matcher for 'Ord' types
cmpSatisfies
  :: (Ord a, Show a, Applicative f)
  => (Ordering -> Bool) -- ^ Predicate matching the outcome of 'compare'.
  -> String             -- ^ Comparison symbol describing the matcher.
  -> a                  -- ^ Value to compare against.
  -> MatcherF f a
cmpSatisfies p symbol bound = simpleMatcher (\x -> p $ compare x bound) message
  where message = hsep ["a", "value", pretty symbol, display bound]

-- | Matcher that always succeeds.
anything :: (Show a, Applicative f) => MatcherF f a
anything = simpleMatcher (const True) "anything"

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
allOf = aggregateWith and "all of"

-- | Constructs a matcher that succeed if at least one of the matchers
-- in the provided list succeed.
oneOf :: (Show a, Applicative f) => MatcherSetF f a -> MatcherF f a
oneOf = aggregateWith or "one of"

-- | Inverts the outcome of the given matcher.
doNotMatch :: (Show a, Applicative f)
           => MatcherF f a -- ^ The matcher to inverse.
           -> MatcherF f a
doNotMatch m = aggregateMatcher (all not) "not" [m]

-- | A version of 'allOf' specialized for two submatchers.
andAlso :: (Show a, Applicative f) => MatcherF f a -> MatcherF f a -> MatcherF f a
andAlso l r = allOf $ matchers [l, r]

-- | A version of 'oneOf' specialized for two submatchers.
orElse :: (Show a, Applicative f) => MatcherF f a -> MatcherF f a -> MatcherF f a
orElse l r = oneOf $ matchers [l, r]

-- | Checks that the container has no values.
isEmpty :: (Show (t a), Foldable t, Applicative f) => MatcherF f (t a)
isEmpty = simpleMatcher null "is empty"

-- | Checks that elements of the container are matched by the matchers
-- exactly. The number of elements in the container should match the
-- number of matchers in the list.
elementsAre :: (Foldable t, Monad f, Show a, Show (t a))
            => [MatcherF f a]   -- ^ List of matchers for the elements of the list.
            -> MatcherF f (t a) -- ^ Matcher for the whole container.
elementsAre matchers = maybe emptyTree mkTree
  where
    emptyTree = Node False name noValueMessage <$> sequenceA (map ($ Nothing) matchers)

    mkTree fitems = do
      items <- fitems
      subnodes <- sequenceA $ go (toList items) matchers 0
      return $ Node (all nodeValue subnodes) name (display items) subnodes

    name = "elements are"
    sizeMessage n = hsep ["contains", "exactly", display n, "elements"]

    go (x:xs) (m:ms) n = m (Just $ pure x) : go xs ms (n + 1)
    go [] [] n = [pure $ Node True (sizeMessage n) (display n) []]
    go moreItems@(x:_) [] n = [pure $ Node False "tail is empty" (display moreItems) []]
    go [] ms@(m:_) n = [ m Nothing
                       , pure $ Node False (sizeMessage (length ms + n)) (display n) []
                       ]

-- | Matcher that succeeds if the argument starts with the specified
-- prefix.
startsWith :: (Applicative f, Show a, Eq a)
           => [a]            -- ^ The prefix the list is expected to have.
           -> MatcherF f [a]
startsWith xs = simpleMatcher (xs `isPrefixOf`)
                (hsep ["starts with", display xs])

-- | Matcher that succeeds if the argument ends with the specified
-- suffix.
endsWith :: (Applicative f, Show a, Eq a)
           => [a]            -- ^ The suffix the list is expected to have.
           -> MatcherF f [a]
endsWith xs = simpleMatcher (xs `isSuffixOf`)
              (hsep ["ends with", display xs])

-- | Matcher that succeeds if the argument contains the given infix.
hasInfix :: (Applicative f, Show a, Eq a)
           => [a]            -- ^ The infix the list is expected to have.
           -> MatcherF f [a]
hasInfix xs = simpleMatcher (xs `isInfixOf`)
              (hsep ["has infix", display xs])

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
property name proj m = aggregateMatcher and msg [contramap proj m]
  where msg = hsep ["property", pretty name, "is"]

-- | Builds a matcher for one alternative of a sum type given matcher for a
-- prism projection.
prism :: (Show s, Show a, Traversable f, Applicative f)
      => String         -- ^ The name of the selected alternative of the structure 's'.
      -> (s -> Maybe a) -- ^ The projection that tries to select the alternative 'a'.
      -> MatcherF f a   -- ^ The matcher for the value of the alternative.
      -> MatcherF f s
prism name p m v =
  case v of
    Nothing -> (Node False msg noValueMessage . pure) <$> m Nothing
    Just fs ->
      (\n s -> Node (nodeValue n) msg (display s) [n])
      <$> m (sequenceA $ fmap p fs)
      <*> fs
  where
    msg = hsep ["prism", pretty name, "is"]

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
      msg = hsep [ "exception", "of", "type", excName, "matches"]
  case maybeAction of
    Nothing ->
      return $ Node False msg noValueMessage [runIdentity $ exMatcher Nothing]
    Just action -> do
      outcome <- tryExn action
      case outcome of
        NoExn -> return $ Node False msg noValueMessage [runIdentity $ exMatcher Nothing]
        ExpectedExn exn -> return $ let node = match exn exMatcher
                                    in Node (nodeValue node) msg (display exn) [node]
        OtherExn exn -> return $ Node False msg (display exn) [runIdentity $ exMatcher Nothing]

-- | Runs a pure matcher on the given value and returns the
-- 'MatchTree'.
match
  :: (Show a)
  => a -- ^ Value to match.
  -> Matcher a -- ^ Matcher to run on the value.
  -> MatchTree
match x p = runIdentity $ p (Just $ Identity x)

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
