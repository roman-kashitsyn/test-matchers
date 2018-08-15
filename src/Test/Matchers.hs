{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |
Module:       Test.Matchers
Description:  Simple matcher combinators for simpler unit testing.
Copyright:    (c) Roman Kashitsyn, 2018
License:      Apache2
Maintainer:   roman.kashitsyn@gmail.com
Stability:    experimental

This module provides simple matcher combinators to simplify
unit-testing and make tests easier to debug.

-}
module Test.Matchers
  ( MatcherF
  , Matcher
  , Message

  , MatchTree(..)

  , simpleMatcher
  , aggregateMatcher

  -- * Matchers for Eq types
  , eq
  , lt
  , gt

  -- * Matchers combinators
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
  , containsInOrder

  -- * Matchers for basic types from the standard library
  , tuple2
  , leftIs
  , rightIs
  , property
  , prism

  -- * Exception matching
  , throws

  -- * HUnit integration
  , shouldMatch
  , shouldMatchIO
  , match
  ) where

import           Control.Exception     (Exception (..), Handler (..),
                                        SomeException, catches)
import           Data.Foldable         (Foldable, null, toList)
import           Data.Functor.Identity (Identity (..), runIdentity)
import           Data.Typeable         (typeOf)
import           Data.List             (isPrefixOf, isInfixOf, isSuffixOf)
import           Test.HUnit            (Assertion, assertFailure)

import           Text.PrettyPrint      (($+$), (<+>), (<>))
import qualified Text.PrettyPrint      as PP

type Message = PP.Doc

-- | Matcher is a function mapping optional values to match trees.
-- Nothing means that there's no value to match. This is not really
-- useful for the users directly but allows to construct meaningful
-- messages when some parts of the structure being matched is missing.
type MatcherF f a = Maybe (f a) -> f MatchTree

-- | A specialization of the 'MatcherF' that can only match pure
-- values.
type Matcher a    = MatcherF Identity a

-- | The result of a matcher invokation.
data MatchTree
  = Node
    { nodeValue        :: !Bool       -- ^ Whether the match was successful.
    , nodeMessage      :: Message     -- ^ Message describing this matcher.
    , nodeMatchedValue :: Message     -- ^ String representation of the value matched.
    , nodeSubnodes     :: [MatchTree] -- ^ Submatchers used to produce this result.
    } deriving (Show, Eq)

noValueMessage :: Message
noValueMessage = "nothing"

inputToDocF :: (Applicative f, Show a) => Maybe (f a) -> f Message
inputToDocF Nothing   = pure noValueMessage
inputToDocF (Just fa) = toDoc <$> fa

inputToDoc :: (Show a) => Maybe a -> Message
inputToDoc = maybe noValueMessage toDoc

-- | Makes a matcher from a predicate and it's description.
simpleMatcher :: (Show a, Applicative f)
              => (a -> Bool) -- ^ Predicate to use for matching
              -> Message     -- ^ Message describing the predicate
              -> MatcherF f a
simpleMatcher predicate msg v =
  case v of
    Nothing -> pure $ Node False msg noValueMessage []
    Just fa -> (\x -> Node (predicate x) msg (inputToDoc (Just x)) []) <$> fa

-- | Makes a matcher that aggregates results of submatchers.
aggregateMatcher :: (Show a, Applicative f)
                 => ([Bool] -> Bool) -- ^ Subnode result aggregation function
                 -> Message          -- ^ Message describing this matcher
                 -> [MatcherF f a]   -- ^ Matchers to aggregate
                 -> MatcherF f a
aggregateMatcher aggF msg ms = doMatch
  where
    doMatch x = mkAgg <$> (inputToDocF x) <*> (sequenceA $ map ($ x) ms)
    mkAgg val nodes = Node (aggF $ map nodeValue nodes) msg val nodes

-- | Matcher that succeeds if the argument equals to the specified
-- value.
eq :: (Eq a, Show a, Applicative f)
   => a          -- ^ The value that the argument must be equal to
   -> MatcherF f a
eq value = simpleMatcher (== value) message
  where message = PP.hsep ["a", "value", "equal", "to", toDoc value]

-- | Mathcer that succeeds if the argument is /greater than/ the
-- specified value.
gt :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
gt = cmpSatisfies (== GT) ">"

-- | Mathcer that succeeds if the argument is /less than/ the
-- specified value.
lt :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
lt = cmpSatisfies (== LT) "<"

-- | A helper function to contruct matcher for Ord types
cmpSatisfies :: (Ord a, Show a, Applicative f)
             => (Ordering -> Bool) -- ^ Predicate matching the outcome of 'compare'
             -> String             -- ^ Comparison symbol describing the matcher
             -> a                  -- ^ Value to compare against
             -> MatcherF f a
cmpSatisfies p symbol bound = simpleMatcher (\x -> p $ compare x bound) message
  where message = PP.hsep ["a", "value", PP.text symbol, toDoc bound]

-- | Matcher that always succeeds.
anything :: (Show a, Applicative f) => MatcherF f a
anything = simpleMatcher (const True) "anything"

-- | Constructs a matcher that succeed if all the matchers in the
-- provided list succeed.
allOf :: (Show a, Applicative f) => [MatcherF f a] -> MatcherF f a
allOf = aggregateMatcher and "all of"

-- | Constructs a matcher that succeed if at least one of the matchers
-- in the provided list succeed.
oneOf :: (Show a, Applicative f) => [MatcherF f a] -> MatcherF f a
oneOf = aggregateMatcher or "one of"

-- | Inverts the outcome of the given matcher.
doNotMatch :: (Show a, Applicative f)
           => MatcherF f a -- ^ The matcher to inverse.
           -> MatcherF f a
doNotMatch m = aggregateMatcher (and . map not) "not" [m]

-- | A version of 'allOf' specialized for two submatchers.
andAlso :: (Show a, Applicative f) => MatcherF f a -> MatcherF f a -> MatcherF f a
andAlso l r = allOf [l, r]

-- | A version of 'oneOf' specialized for two submatchers.
orElse :: (Show a, Applicative f) => MatcherF f a -> MatcherF f a -> MatcherF f a
orElse l r = oneOf [l, r]

-- | Checks that the container has no values.
isEmpty :: (Show (t a), Foldable t, Applicative f) => MatcherF f (t a)
isEmpty = simpleMatcher null "is empty"

-- | Checks that elements of the container match the matchers exactly.
elementsAre :: (Foldable t, Monad f, Show a, Show (t a))
            => [MatcherF f a]
            -> MatcherF f (t a)
elementsAre matchers = maybe emptyTree mkTree
  where
    emptyTree = Node False name noValueMessage <$> (sequenceA $ map ($ Nothing) matchers)

    mkTree fitems = do
      items <- fitems
      subnodes <- sequenceA $ go (toList items) matchers 0
      return $ Node (and $ map nodeValue subnodes) name (toDoc items) subnodes

    name = "elements are"
    sizeMessage n = PP.hsep ["contains", "exactly", toDoc n, "elements"]

    go (x:xs) (m:ms) n = (m $ Just $ pure x) : go xs ms (n + 1)
    go [] [] n = [pure $ Node True (sizeMessage n) (toDoc n) []]
    go moreItems@(x:_) [] n = [pure $ Node False "tail is empty" (toDoc moreItems) []]
    go [] ms@(m:_) n = [ m Nothing
                       , pure $ Node False (sizeMessage (length ms + n)) (toDoc n) []
                       ]

-- | Matcher that succeeds if the argument starts with the specified
-- prefix.
startsWith :: (Applicative f, Show a, Eq a)
           => [a]
           -> MatcherF f [a]
startsWith xs = simpleMatcher (xs `isPrefixOf`)
                (PP.hsep ["starts with", toDoc xs])

-- | Matcher that succeeds if the argument ends with the specified
-- suffix.
endsWith :: (Applicative f, Show a, Eq a)
           => [a]
           -> MatcherF f [a]
endsWith xs = simpleMatcher (xs `isSuffixOf`)
              (PP.hsep ["ends with", toDoc xs])

-- | Matcher that succeeds if the argument contains the given infix.
containsInOrder :: (Applicative f, Show a, Eq a)
           => [a]
           -> MatcherF f [a]
containsInOrder xs = simpleMatcher (xs `isInfixOf`)
                     (PP.hsep ["contains in order", toDoc xs])

-- | Builds a matcher for a pair from the matchers of components.
tuple2 :: (Show a, Show b, Applicative f) => MatcherF f a -> MatcherF f b -> MatcherF f (a, b)
tuple2 mx my = (property "fst" fst mx) `andAlso` (property "snd" snd my)

-- | Makes a matcher that only matches Left values satisfying given
-- matcher.
leftIs :: (Show a, Show b, Monad f)
       => MatcherF f a -- ^ Matcher of a left value
       -> MatcherF f (Either a b)
leftIs  = prism "Left"  $ \x -> case x of { Left a  -> Just a; _ -> Nothing }

-- | Makes a matcher that only matches Right values satisfying given
-- matcher.
rightIs :: (Show a, Show b, Monad f)
        => MatcherF f b -- ^ Matcher of a right value
        -> MatcherF f (Either a b)
rightIs = prism "Right" $ \x -> case x of { Right b -> Just b; _ -> Nothing }

contramap :: (Functor f) => (s -> a) -> MatcherF f a -> MatcherF f s
contramap f p = p . fmap (fmap f)

-- | Builds a matcher for a structure from a matcher of its substructure.
--
-- It's equivalent to 'contramap' but also takes a name for
-- mismatch reporting.
property :: (Show a, Show s, Applicative f)
         => String      -- ^ Name of the property of the structure 's'
         -> (s -> a)    -- ^ The projection from a structure 's' to it's substructure 'a'
         -> (MatcherF f a) -- ^ Matcher of the substructure 'a'
         -> (MatcherF f s)
property name proj m = aggregateMatcher and msg [contramap proj m]
  where msg = PP.hsep ["property", PP.text name, "is"]

prism :: (Show s, Show a, Monad f)
      => String
      -> (s -> Maybe a)
      -> MatcherF f a
      -> MatcherF f s
prism name p m v =
  case v of
    Nothing -> (Node False msg noValueMessage . pure) <$> (m Nothing)
    Just fs -> do
      s <- fs
      let maybeA = p s
      (\n -> Node (nodeValue n) msg (toDoc s) [n]) <$> m (fmap pure maybeA)
  where
    msg = PP.hsep ["prism", PP.text name, "is"]

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

-- | Checks that an action throws an exception that matches the
-- provided matcher.
throws :: forall e a. (Exception e)
       => Matcher e     -- ^ Matcher of the exception.
       -> MatcherF IO a
throws exMatcher maybeAction = do
  let excName = toDoc $ typeOf (undefined :: e)
      msg = PP.hsep [ "exception", "of", "type", excName, "matches"]
  case maybeAction of
    Nothing -> do
      return $ Node False msg noValueMessage [runIdentity $ exMatcher $ Nothing]
    Just action -> do
      outcome <- tryExn action
      case outcome of
        NoExn -> return $ Node False msg noValueMessage [runIdentity $ exMatcher Nothing]
        ExpectedExn exn -> return $ let node = match exn exMatcher
                                    in Node (nodeValue node) msg (toDoc exn) [node]
        OtherExn exn -> return $ Node False msg (toDoc exn) [runIdentity $ exMatcher Nothing]

treeToAssertion :: MatchTree -> Assertion
treeToAssertion tree = if nodeValue tree
                       then return ()
                       else assertFailure (prettyPrint tree)

-- | Checks that a pure value is matched by the given matcher.
-- The function is designed to be used in test frameworks, mainly HUnit and
-- HSpec. If the value doesn't satisfy the matcher, the test will fail and
-- print the detailed match tree.
--
-- @
-- testCase = TestCase (fib 5 `shouldMatch` (eq 5))
-- @
shouldMatch :: (Show a)
            => a          -- ^ a value to match
            -> Matcher a  -- ^ matcher to run
            -> Assertion
shouldMatch x m = treeToAssertion (match x m)

-- | A variant of 'shouldMatch' that matches an IO action instead of a
-- pure value.
--
-- @
-- readNonExistingFile `shouldMatchIO` throws (anything :: Matcher IOException)
-- @
shouldMatchIO :: IO a          -- ^ an action to match
              -> MatcherF IO a -- ^ a matcher of the action
              -> Assertion
shouldMatchIO action matcher = matcher (Just action) >>= treeToAssertion

match :: (Show a) => a -> Matcher a -> MatchTree
match x p = runIdentity $ p (Just $ Identity x)

check, cross, arrow :: PP.Doc
check = "☑"
cross = "☒"
arrow = "←"

-- | Pretty-prints a matching tree.
treeToDoc :: MatchTree -> PP.Doc
treeToDoc (Node res msg val subnodes) =
  (if res then check else cross) <+>
  PP.hsep [msg, arrow, val] $+$
  PP.nest 2 (foldr ($+$) PP.empty $ map treeToDoc subnodes)

prettyPrint :: MatchTree -> String
prettyPrint = PP.render . treeToDoc

toDoc :: (Show a) => a -> Message
toDoc = PP.text . show
