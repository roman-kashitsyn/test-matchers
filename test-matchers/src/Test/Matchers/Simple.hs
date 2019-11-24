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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_HADDOCK hide #-}
{-
|
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
  , Direction(..)
  , predicate

  -- * Matchers for 'Eq' and 'Ord' types
  , eq
  , ne

  , lt
  , le
  , gt
  , ge

  -- * Matchers for numeric types
  , numberNear
  , floatApproxEq

  -- * Matchers combinators
  , anything
  , projection
  , prism
  , allOf
  , oneOf
  , negationOf
  , andAlso
  , orElse
  , contramap
  , contramapSet
  , labeled

  -- * Matchers for sequences
  , isEmpty
  , isNotEmpty
  , hasLength
  , each
  , elementsAre
  , startsWith
  , endsWith
  , hasInfix

  -- * Matchers for basic types from the standard library
  , tuple2
  , tuple3
  , isNothing
  , isJustWith
  , isLeftWith
  , isRightWith

  -- * Matchers for exceptions
  , throws

  -- * Executing matchers
  , runMatcher
  , match
  , matches

  -- * Operators
  , (&.)
  , (&>)
  ) where

import Control.Applicative (liftA2)
import Control.Exception (Exception(..), Handler(..), SomeException, catches)
import Data.Foldable (Foldable, null, toList)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Traversable (Traversable)
import Data.Typeable (typeOf)
import Test.Matchers.Message (Message, display, fancyChar, hsep, str, symbol)

-- | The direction matcher works in.
data Direction
  = Positive -- ^ The predicate of the matcher should be applied as is.
  | Negative -- ^ The predicate of the matcher should be negated.
  deriving (Show, Eq, Bounded, Enum)

-- | Matcher is a function from optional values to match trees.
-- 'Nothing' means that there's no value to match. This is not useful
-- directly but allows the library to construct meaningful messages
-- when some parts of the structure being matched are missing.  The
-- @f@ parameter is there so that we can match not just pure values
-- but IO actions as well, see the 'throws' combinator as an example.
newtype MatcherF f a = MatcherF { unMatcherF :: Direction -> Maybe (f a) -> f MatchTree }

-- | A (possibly empty) group of matchers. A set of matchers can be
-- turned into a proper matcher via aggregation functions,
-- e.g. 'allOf' or 'oneOf'.
--
-- Matcher sets can be composed \"sequentially\" or in
-- \"parallel\".
--
-- \"Parallel\" composition combines @[MatcherF f a]@ and
-- @MatcherSetF f b@ into @MatcherSetF f (a, b)@ and is achieved via the
-- '&>' operator.
--
-- @
--
--         ╭───╮                 ╭───╮               ╭───────╮
--         │ a │       &.        │ b │       =       │ (a,b) │
--         ╰─┬─╯                 ╰─┬─╯               ╰───┬───╯
--           ╽                     ╽                     ╽
--  ╭────────────────╮    ╭────────────────╮   ╭────────────────────╮
--  │ [MatcherF f a] │ &> │ [MatcherF f b] │ = │ [MatcherF f (a,b)] │
--  ╰────────────────╯    ╰────────────────╯   ╰────────────────────╯
-- @
--
-- \"Sequential\" composition combines multiple @[MatcherF f a]@ into one
-- and is achieved via 'mappend'.
--
-- @
--                    ╭───╮
--                    │ a │
--                    ╰─┬─╯
--  ╭────────────────╮  │
--  │ [MatcherF f a] │ ╾┤
--  ╰────────────────╯  │
--        mappend       │
--  ╭────────────────╮  │
--  │ [MatcherF f a] │ ╾┤
--  ╰────────────────╯  │
--           =          │
--  ╭────────────────╮  │
--  │ [MatcherF f a] │ ╾╯
--  ╰────────────────╯
-- @
matchSetF :: Applicative f => [MatcherF f a] -> (Direction -> Maybe (f a) -> f [MatchTree])
matchSetF set =
    \dir m -> traverse (\(MatcherF f) -> f dir m) set

-- | A specialization of the 'MatcherF' that can only match pure
-- values.
type Matcher a    = MatcherF Identity a

-- | The result of a matcher invocation.
data MatchTree
  = MatchTree
    { mtValue        :: !Bool -- ^ Whether the match was successful.
    , mtDescription  :: Message -- ^ Message describing this matcher.
    , mtLabels       :: [String] -- ^ List of labels attached to the matcher.
    , mtMatchedValue :: Maybe String -- ^ Textual representation of the value matched.
    , mtSubnodes     :: [MatchTree] -- ^ Submatchers used to produce this result.
    } deriving (Show, Eq)

makeFullTree
  :: (Show a)
  => Bool
  -> Message
  -> a
  -> [MatchTree]
  -> MatchTree
makeFullTree r m v = MatchTree r m [] (Just $ show v)

makeEmptyTree
  :: Message
  -> [MatchTree]
  -> MatchTree
makeEmptyTree m = MatchTree False m [] Nothing

transformTree
  :: (Functor f)
  => (MatchTree -> MatchTree)
  -> MatcherF f a
  -> MatcherF f a
transformTree f = MatcherF . fmap (fmap (fmap f)) . unMatcherF

-- | Changes the direction to it's opposite.
flipDirection :: Direction -> Direction
flipDirection Positive = Negative
flipDirection Negative = Positive

-- | Applies the given direction to the output of the predicate.
applyDirection :: Direction -> Bool -> Bool
applyDirection Positive = id
applyDirection Negative = not

-- | Picks the description corresponding to the direction.
pickDescription :: Direction -> (Message, Message) -> Message
pickDescription Positive = fst
pickDescription Negative = snd

-- | Makes a matcher from a predicate and it's description.
predicate
  :: (Show a, Applicative f)
  => (a -> Bool) -- ^ Predicate to use for matching
  -> (Message, Message) -- ^ Messages describing this predicate and it's negationOf.
  -> MatcherF f a
predicate p descr
  = MatcherF $ \dir v ->
                 let msg = pickDescription dir descr
                 in case v of
                      Nothing -> pure $ makeEmptyTree msg []
                      Just fa -> (\x -> makeFullTree (applyDirection dir (p x)) msg x []) <$> fa

aggregateWith
  :: (Show a, Applicative f)
  => ([Bool] -> Bool)
  -> (Message, Message)
  -> [MatcherF f a]
  -> MatcherF f a
aggregateWith aggr descr matcherSet
  = MatcherF $ \dir value ->
                 let subnodesF = matchSetF matcherSet Positive value
                     msgF = traverse (fmap show) value
                 in liftA2 (\xs m -> MatchTree
                             (applyDirection dir (aggr $ map mtValue xs))
                             (pickDescription dir descr)
                             []
                             m xs)
                    subnodesF msgF


-- | Matcher that succeeds if the argument equals to the specified
-- value.
eq :: (Eq a, Show a, Applicative f)
   => a -- ^ The value that the argument must be equal to
   -> MatcherF f a
eq value = predicate (== value) (descr, descr_)
  where descr  = hsep ["is", "a", "value", "equal", "to", display value]
        descr_ = hsep ["is", "a", "value", "not", "equal", "to", display value]

-- | Matcher that succeeds if the argument is not equal to the specified value.
--
-- This matcher is the complement of 'eq'.
ne :: (Eq a, Show a, Applicative f)
   => a -- ^ The value that the argument must be not equal to.
   -> MatcherF f a
ne = negationOf . eq

-- | Matcher that succeeds if the argument (which is a number) is not
-- further than the specified absolute error from the given value.
--
-- Note that NaNs are considered not equal, not close and not far.
numberNear
  :: (Num a, Ord a, Show a, Applicative f)
  => a -- ^ The maximum absolute error.
  -> a -- ^ The value argument of the matcher must be close to.
  -> MatcherF f a
numberNear absError value = predicate (\x -> abs (value - x) <= absError)
                           (descr, descr_)
  where descr  = hsep ["is", "not", "further", "than", display absError
                      , "from", display value]
        descr_ = hsep ["is", "further", "than", display absError
                      , "from", display value]

-- | Matchers that succeeds if the argument which is a floating point
-- number is relatively close to the specified value.
--
-- If you match against zero, consider using 'floatNear' instead.
--
-- Note that NaNs are not considered equal.
--
-- See https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
floatApproxEq
  :: (RealFloat a, Show a, Applicative f)
  => a -- ^ The value that the argument must be almost equal to.
  -> MatcherF f a
floatApproxEq value = predicate approxEq (descr, descr_)
  where
    -- ULP-based comparison, see the link above for details.
    approxEq x = signum value == signum x
                 && diffULP value x <= maxULPDiff
    -- Computes the number of floats between y and z
    diffULP y z = let (by, ey) = decodeFloat y
                      (bz, ez) = decodeFloat z
                      radix = floatRadix y
                  in abs $ if ey >= ez
                           then by * (radix ^ (ey - ez)) - bz
                           else bz * (radix ^ (ez - ey)) - by
    maxULPDiff = 4
    descr  = hsep ["is", "a", "value", "approx.", "equal", "to", display value]
    descr_ = hsep ["is", "a", "value", "not", "equal", "to", display value]

-- | Mathcer that succeeds if the argument is /greater than/ the
-- specified value.
gt :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
gt = cmpSatisfies (== GT) (str ">") (fancyChar '≤' "<=")

-- | Mathcer that succeeds if the argument is /greater than or equal to/
-- the specified value.
ge :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
ge = negationOf . lt

-- | Mathcer that succeeds if the argument is /less than/ the
-- specified value.
lt :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
lt = cmpSatisfies (== LT) (str "<") (fancyChar '≥' ">=")

-- | Mathcer that succeeds if the argument is /less than or equal to/
-- the specified value.
le :: (Ord a, Show a, Applicative f)
   => a
   -> MatcherF f a
le = negationOf . gt

-- | A helper function to contruct matcher for 'Ord' types
cmpSatisfies
  :: (Ord a, Show a, Applicative f)
  => (Ordering -> Bool) -- ^ Predicate matching the outcome of 'compare'.
  -> Message -- ^ Comparison symbol describing the matcher.
  -> Message -- ^ Comparison symbol describing the negationOf of the matcher.
  -> a -- ^ Value to compare against.
  -> MatcherF f a
cmpSatisfies p sym sym_ bound = predicate (\x -> p $ compare x bound) (descr, descr_)
  where descr  = hsep ["is", "a", "value", sym,  display bound]
        descr_ = hsep ["is", "a", "value", sym_, display bound]

-- | Matcher that always succeeds.
anything :: (Show a, Applicative f) => MatcherF f a
anything = predicate (const True) ("anything", "nothing")

-- | Constructs a matcher that succeed if all the matchers in the
-- provided set succeed.
allOf
  :: ( Show a
     , Applicative f
     )
  => [MatcherF f a]
  -> MatcherF f a
allOf = aggregateWith and ("all of", "not all of")

-- | A more convenient version of 'oneOfSet' that works on foldable
-- containers instead of matcher sets.
oneOf
  :: ( Show a
     , Applicative f
     )
  => [MatcherF f a]
  -> MatcherF f a
oneOf = aggregateWith or ("one of", "none of")

-- | Inverts the given matcher.
--
-- prop> forall m. negationOf (negationOf m) == m
-- prop> forall x m. x matches m ⇒ not (x matches (negationOf m))
negationOf
  :: MatcherF f a -- ^ The matcher to negationOf.
  -> MatcherF f a
negationOf m = MatcherF $ \d -> unMatcherF m (flipDirection d)

-- | A version of 'allOf' specialized for two submatchers.
andAlso
  :: (Show a, Applicative f)
  => MatcherF f a
  -> MatcherF f a
  -> MatcherF f a
andAlso l r = allOf [l, r]

-- | A version of 'oneOf' specialized for two submatchers.
orElse
  :: (Show a, Applicative f)
  => MatcherF f a
  -> MatcherF f a
  -> MatcherF f a
orElse l r = oneOf [l, r]

-- | Checks that the container has no values.
-- The negationOf is 'isNotEmpty'.
isEmpty
  :: (Show (t a), Foldable t, Applicative f)
  => MatcherF f (t a)
isEmpty = predicate null ("is empty", "is not empty")

-- | The complement of 'isEmpty'.
isNotEmpty
  :: (Show (t a), Foldable t, Applicative f)
  => MatcherF f (t a)
isNotEmpty = negationOf isEmpty

-- | Checks that container length satisfies the given matcher.
hasLength
  :: (Show (t a), Foldable t, Applicative f)
  => MatcherF f Int -- ^ Matcher for the container size.
  -> MatcherF f (t a)
hasLength m = projection "length" length [m]

-- | checks that each element of the container matches the provided
-- matcher for container elements.  If the container is empty, the
-- matcher still succeeds.
--
-- The negation of this matcher checks that container has at least one
-- counter-example for the given element matcher.
each
  :: (Foldable t, Monad f, Show (t a))
  => MatcherF f a -- ^ Matcher for the elements of the container.
  -> MatcherF f (t a) -- ^ Matcher for the whole container.
each m
  = MatcherF $ \dir val ->
                 let
                   descr = pickDescription dir (descrPos, descrNeg)
                   descrPos = hsep ["each", "element", "of", "the", "container"]
                   descrNeg = hsep ["container", "has", "at", "least", "one", "element", "that"]
                   mkEmpty outcome showedVal t = makeFullTree outcome descr showedVal [t]
                   fTree = unMatcherF m Positive Nothing
                 in case val of
                      Nothing -> makeEmptyTree descr . pure <$> fTree
                      Just fta -> do
                        ta <- fta
                        if null ta
                          then mkEmpty (applyDirection dir True) ta <$> fTree
                          else do subtrees <- traverse (runMatcher m . pure) (toList ta)
                                  pure $ makeFullTree
                                    (applyDirection dir $ all mtValue subtrees)
                                    descr ta subtrees
-- | Checks that elements of the container are matched by the matchers
-- exactly. The number of elements in the container should match the
-- number of matchers in the list.
elementsAre
  :: (Foldable t, Monad f, Show (t a))
  => [MatcherF f a] -- ^ List of matchers for the elements of the list.
  -> MatcherF f (t a) -- ^ Matcher for the whole container.
elementsAre matcherList
  = MatcherF $ \dir ->
                 let
                   emptyTree = makeEmptyTree descr <$> traverse (\m -> unMatcherF m Positive Nothing) matcherList

                   mkTree fitems = do
                     items <- fitems
                     subnodes <- sequenceA $ go (toList items) matcherList 0
                     return $ makeFullTree (applyDirection dir (all mtValue subnodes)) name items subnodes

                   numMatchers = length matcherList
                   name  = hsep ["container", "such", "that"]
                   name_ = hsep ["container", "such", "that", "not", "all", "of"]
                   descr = pickDescription dir (name, name_)

                   sizeMessage  = hsep ["number", "of", "elements", "is", display numMatchers]

                   go (x:xs) (m:ms) n = runMatcher m (pure x) : go xs ms (n + 1)
                   go [] [] n = [pure $ makeFullTree True sizeMessage n []]
                   go moreItems@(_:_) [] n = [pure $ makeFullTree False sizeMessage (n + length moreItems) []]
                   go [] (m:_) n = [ unMatcherF m Positive Nothing
                                   , pure $ makeFullTree False sizeMessage n []
                                   ]
                 in maybe emptyTree mkTree

-- | Matcher that succeeds if the argument starts with the specified
-- prefix.
startsWith
  :: (Applicative f, Show a, Eq a)
  => [a]            -- ^ The prefix the list is expected to have.
  -> MatcherF f [a]
startsWith xs = predicate (xs `isPrefixOf`)
                ( hsep ["starts", "with", display xs]
                , hsep ["does", "not", "start", "with", display xs]
                )

-- | Matcher that succeeds if the argument ends with the specified
-- suffix.
endsWith
  :: (Applicative f, Show a, Eq a)
  => [a]            -- ^ The suffix the list is expected to have.
  -> MatcherF f [a]
endsWith xs = predicate (xs `isSuffixOf`)
              ( hsep ["ends", "with", display xs]
              , hsep ["does", "not", "end", "with", display xs]
              )

-- | Matcher that succeeds if the argument contains the given infix.
hasInfix
  :: (Applicative f, Show a, Eq a)
  => [a]            -- ^ The infix the list is expected to have.
  -> MatcherF f [a]
hasInfix xs = predicate (xs `isInfixOf`)
              ( hsep ["has", "infix", display xs]
              , hsep ["does", "not", "have", "infix", display xs]
              )

-- | Builds a matcher for a pair from the matchers for components.
tuple2
  :: (Show a, Show b, Applicative f)
  => MatcherF f a -- ^ Matcher for the 1st element of the pair.
  -> MatcherF f b -- ^ Matcher for the 2nd element of the pair.
  -> MatcherF f (a, b)
tuple2 mx my =
  allOf ([labeled "fst" mx] &> [labeled "snd" my])

-- | Builds a matcher for a 3-tuple from the matchers for components.
tuple3
  :: (Show a, Show b, Show c, Applicative f)
  => MatcherF f a -- ^ Matcher for the 1st element of the 3-tuple.
  -> MatcherF f b -- ^ Matcher for the 2nd element of the 3-tuple.
  -> MatcherF f c -- ^ Matcher for the 3rd element of the 3-tuple.
  -> MatcherF f (a, b, c)
tuple3 mx my mz =
  allOf $ contramapSet retuple ([labeled "_1" mx] &>
                                [labeled "_2" my] &>
                                [labeled "_3" mz])
  where retuple (x, y, z) = (x, (y, z))

-- | A matcher for 'Maybe' that matches only 'Nothing'.
isNothing
  :: (Show a, Applicative f)
  => MatcherF f (Maybe a)
isNothing = predicate (\case Nothing -> True; _ -> False)
            ("is Nothing", "is not Nothing")

-- | Builds a matcher for 'Maybe' that only matches 'Just' values
-- satisfying the given matcher.
isJustWith
  :: (Show a, Applicative f, Traversable f)
  => MatcherF f a
  -> MatcherF f (Maybe a)
isJustWith m = prism "Just" id [m]

-- | Makes a matcher that only matches 'Left' values satisfying given
-- matcher.
isLeftWith
  :: (Show a, Show b, Traversable f, Applicative f)
  => MatcherF f a -- ^ Matcher for the left side of 'Either'.
  -> MatcherF f (Either a b)
isLeftWith m = prism "Left" (\case Left a  -> Just a; _ -> Nothing) [m]

-- | Makes a matcher that only matches 'Right' values satisfying given
-- matcher.
isRightWith
  :: (Show a, Show b, Traversable f, Applicative f)
  => MatcherF f b -- ^ Matcher for the right side of 'Either'.
  -> MatcherF f (Either a b)
isRightWith m = prism "Right" (\case Right b -> Just b; _ -> Nothing) [m]

-- | Implementation of Data.Functor.Covariant.contramap.
-- Avoid using it directly, prefer 'projection' instead.
contramap
  :: (Functor f)
  => (s -> a)
  -> MatcherF f a
  -> MatcherF f s
contramap f p = MatcherF $ \dir -> unMatcherF p dir . fmap (fmap f)

-- | Implementation of Data.Functor.Covariant.contramap for a matcher set.
contramapSet
  :: (Functor f)
  => (s -> a)
  -> [MatcherF f a]
  -> [MatcherF f s]
contramapSet = map . contramap

-- | Makes a matcher that attaches a label to the outcome of another
-- matcher.
labeled
  :: (Functor f)
  => String
  -> MatcherF f a
  -> MatcherF f a
labeled l = transformTree addLabel
  where addLabel t = t { mtLabels = l : mtLabels t }

-- | Builds a matcher for a structure from a set of matchers for its substructure.
-- The whole matcher succeeds if all the matchers from the set succeed.
projection
  :: ( Show s
     , Applicative f
     )
  => String -- ^ The name of the projection.
  -> (s -> a) -- ^ The projection from "s" to "a".
  -> [MatcherF f a] -- ^ The set of matchers for the projected "a".
  -> MatcherF f s
projection name proj set = MatcherF $ \dir value ->
  let subnodesF = matchSetF (contramapSet proj set) dir value
      descr  = hsep ["projection", symbol name]
      msgF = traverse (fmap show) value
  in liftA2 (\xs msg -> MatchTree (all mtValue xs) descr [] msg xs)
     subnodesF msgF

-- | Builds a matcher for one alternative of a sum type given matcher for a
-- prism projection. The results of the matchers in the set are
-- aggregated with 'and'.
prism
  :: ( Show s
     , Traversable f
     , Applicative f
     )
  => String -- ^ The name of the selected alternative.
  -> (s -> Maybe a) -- ^ The selector for the alternative "a".
  -> [MatcherF f a] -- ^ The set of matchers for the alternative "a".
  -> MatcherF f s
prism name p set = MatcherF $ \dir v ->
  case v of
    Nothing -> makeEmptyTree descr <$> matchSetF set dir Nothing
    Just fs -> (\subtrees s -> makeFullTree (all mtValue subtrees) descr s subtrees)
      <$> matchSetF set dir (traverse p fs)
      <*> fs
  where descr = hsep ["prism", symbol name]

-- | Represents a result of IO action execution.
data ActionOutcome e
  = NoExn                  -- ^ The action completed successfully.
  | ExpectedExn e          -- ^ The action threw an exception of the expected type.
  | OtherExn SomeException -- ^ The action threw an unexpected exception.

tryExn
  :: forall a e. (Exception e)
  => IO a
  -> IO (ActionOutcome e)
tryExn ma =
  fmap (const NoExn) ma `catches`
  [ Handler (\(exn :: e) -> pure $ ExpectedExn exn)
  , Handler (\(exn :: SomeException) -> pure $ OtherExn exn)
  ]

-- | Checks that an IO action throws an exception satisfying the
-- given matcher.
throws
  :: forall e a. (Exception e)
  => Matcher e  -- ^ Matcher for the exception value.
  -> MatcherF IO a
throws exMatcher = MatcherF $ \dir maybeAction -> do
  let excName = symbol $ typeOf (undefined :: e)
      descr  = hsep ["action", "throwing", excName, "that"]
      descr_  = hsep ["action", "not", "throwing", excName, "that"]
      d = pickDescription dir (descr, descr_)

  case maybeAction of
    Nothing ->
      return $ makeEmptyTree d [runIdentity $ unMatcherF exMatcher Positive Nothing]
    Just action -> do
      outcome <- tryExn action
      case outcome of
        NoExn -> return
          $ makeEmptyTree d [runIdentity $ unMatcherF exMatcher Positive Nothing]
        ExpectedExn exn -> return $ let node = match exMatcher exn
                                    in makeFullTree (applyDirection dir (mtValue node)) d exn [node]
        OtherExn exn -> return $ makeFullTree (applyDirection dir False) d exn
                                 [runIdentity $ unMatcherF exMatcher Positive Nothing]

-- | Runs a matcher on the given effectful value and returns the
-- `MatchTree` wrapped into the same effect.
runMatcher
  :: MatcherF f a -- ^ The matcher to run.
  -> f a -- ^ The effectful value.
  -> f MatchTree -- ^ The resulting match tree.
runMatcher m value = unMatcherF m Positive (Just value)

-- | Runs a pure matcher on the given value and returns the
-- 'MatchTree'.
match
  :: Matcher a -- ^ Matcher to run on the value.
  -> a -- ^ Value to match.
  -> MatchTree
match p x = runIdentity $ runMatcher p (Identity x)

-- | Returns 'True' if the given value matches the provided pure
-- matcher, otherwise returns 'False'.
matches
  :: Matcher a -- ^ Matcher to check
  -> a -- ^ Value to match.
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

infixr 7 &.

-- | Parallel matcher set composition operator, see 'MatcherSetF' for details.
(&>) ::
    ( Show a
    , Show b
    , Applicative f
    )
  => [MatcherF f a]
  -> [MatcherF f b]
  -> [MatcherF f (a, b)]
ma &> mb =
    mappend
    (map (contramap fst) ma)
    (map (contramap snd) mb)

infixr 7 &>
