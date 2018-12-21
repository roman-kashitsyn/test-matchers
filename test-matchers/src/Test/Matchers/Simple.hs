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
  , MatcherSetF
  , matcher
  , matchers
  , anything
  , projection
  , projectionWithSet
  , prism
  , prismWithSet
  , allOf
  , allOfSet
  , oneOf
  , oneOfSet
  , negationOf
  , andAlso
  , orElse
  , contramap
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

import Test.Matchers.Message
  ( Message
  , display
  , fancyChar
  , hsep
  , str
  , symbol
  )
import Control.Applicative   (liftA2)
import Control.Exception
  ( Exception (..)
  , Handler (..)
  , SomeException
  , catches
  )
import Data.Foldable         (Foldable, foldMap, null, toList)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.List             (isInfixOf, isPrefixOf, isSuffixOf)
import Data.Traversable      (Traversable)
import Data.Typeable         (typeOf)

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
type MatcherF f a = Direction -> Maybe (f a) -> f MatchTree

-- | A (possibly empty) group of matchers. A set of matchers can be
-- turned into a proper matcher via aggregation functions,
-- e.g. 'allOfSet' or 'oneOfSet'.
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
newtype MatcherSetF f a = MatcherSetF { matchSetF :: Direction -> Maybe (f a) -> f [MatchTree] }

-- | A specialization of the 'MatcherF' that can only match pure
-- values.
type Matcher a    = MatcherF Identity a

-- | The result of a matcher invocation.
data MatchTree
  = MatchTree
    { mtValue          :: !Bool -- ^ Whether the match was successful.
    , mtDescription    :: Message -- ^ Message describing this matcher.
    , mtLabels         :: [String] -- ^ List of labels attached to the matcher.
    , mtMatchedValue   :: Maybe String -- ^ Textual representation of the value matched.
    , mtSubnodes       :: [MatchTree] -- ^ Submatchers used to produce this result.
    } deriving (Show, Eq)

instance (Applicative f) => Monoid (MatcherSetF f a) where
  mempty = MatcherSetF $ const $ const $ pure []
  mappend (MatcherSetF l) (MatcherSetF r) = MatcherSetF $ \dir x -> liftA2 mappend (l dir x) (r dir x)

makeFullTree
  :: (Show a)
  => Bool
  -> Message
  -> a
  -> [MatchTree]
  -> MatchTree
makeFullTree r m v sn = MatchTree r m [] (Just $ show v) sn

makeEmptyTree
  :: Message
  -> [MatchTree]
  -> MatchTree
makeEmptyTree m sn = MatchTree False m [] Nothing sn

transformTree
  :: (Functor f)
  => (MatchTree -> MatchTree)
  -> MatcherF f a
  -> MatcherF f a
transformTree f = fmap (fmap (fmap f))

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
predicate p descr dir v =
  case v of
    Nothing -> pure $ makeEmptyTree msg []
    Just fa -> (\x -> makeFullTree (applyDirection dir (p x)) msg x []) <$> fa
  where msg = pickDescription dir descr


aggregateWith :: (Show a, Applicative f)
              => ([Bool] -> Bool)
              -> (Message, Message)
              -> MatcherSetF f a
              -> MatcherF f a
aggregateWith aggr descr matcherSet dir value =
  let subnodesF = matchSetF matcherSet Positive value
      msgF = sequenceA $ fmap (fmap show) value
  in liftA2 (\xs m -> MatchTree
                      (applyDirection dir (aggr $ map mtValue xs))
                      (pickDescription dir descr)
                      []
                      m xs)
     subnodesF msgF


-- | Matcher that succeeds if the argument equals to the specified
-- value.
eq
  :: (Eq a, Show a, Applicative f)
  => a -- ^ The value that the argument must be equal to
  -> MatcherF f a
eq value = predicate (== value) (descr, descr_)
  where descr  = hsep ["is", "a", "value", "equal", "to", display value]
        descr_ = hsep ["is", "a", "value", "not", "equal", "to", display value]

-- | Matcher that succeeds if the argument is not equal to the specified value.
--
-- This matcher is the complement of 'eq'.
ne
  :: (Eq a, Show a, Applicative f)
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
matcher = MatcherSetF . fmap (fmap (fmap pure))

-- | Constructs a matcher that succeed if all the matchers in the
-- provided set succeed.
allOfSet
  :: (Show a, Applicative f)
  => MatcherSetF f a
  -> MatcherF f a
allOfSet = aggregateWith and ("all of", "not all of")

-- | A more convenient version of 'allOfSet' that works on foldable
-- containers instead of matcher sets.
allOf
  :: (Show a, Applicative f, Foldable t)
  => t (MatcherF f a) -- ^ A foldable container with matchers.
  -> MatcherF f a
allOf = allOfSet . matchers

-- | Constructs a matcher that succeed if at least one of the matchers
-- in the provided set succeed.
oneOfSet
  :: (Show a, Applicative f)
  => MatcherSetF f a
  -> MatcherF f a
oneOfSet = aggregateWith or ("one of", "none of")

-- | A more convenient version of 'oneOfSet' that works on foldable
-- containers instead of matcher sets.
oneOf
  :: (Show a, Applicative f, Foldable t)
  => t (MatcherF f a) -- ^ A foldable container with matchers.
  -> MatcherF f a
oneOf = oneOfSet . matchers

-- | Inverts the given matcher.
--
-- prop> forall m. negationOf (negationOf m) == m
-- prop> forall x m. x matches m ⇒ not (x matches (negationOf m))
negationOf
  :: MatcherF f a -- ^ The matcher to negationOf.
  -> MatcherF f a
negationOf m d = m (flipDirection d)

-- | A version of 'allOf' specialized for two submatchers.
andAlso :: (Show a, Applicative f) => MatcherF f a -> MatcherF f a -> MatcherF f a
andAlso l r = allOf [l, r]

-- | A version of 'oneOf' specialized for two submatchers.
orElse :: (Show a, Applicative f) => MatcherF f a -> MatcherF f a -> MatcherF f a
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
hasLength = projection "length" length

-- | Checks that each element of the container matches the provided
-- matcher for container elements.  If the container is empty, the
-- matcher still succeeds.
--
-- The negation of this matcher checks that container has at least one
-- counter-example for the given element matcher.
each
  :: (Foldable t, Monad f, Show (t a))
  => MatcherF f a -- ^ Matcher for the elements of the container.
  -> MatcherF f (t a) -- ^ Matcher for the whole container.
each m dir val =
  case val of
    Nothing -> makeEmptyTree descr . pure <$> fTree
    Just fta -> do
      ta <- fta
      if null ta
        then mkEmpty (applyDirection dir True) ta <$> fTree
        else do subtrees <- traverse (m Positive . Just . pure) (toList ta)
                pure $ makeFullTree
                       (applyDirection dir $ all mtValue subtrees)
                       descr ta subtrees

  where descr = pickDescription dir (descrPos, descrNeg)
        descrPos = hsep ["each", "element", "of", "the", "container"]
        descrNeg = hsep ["container", "has", "at", "least", "one", "element", "that"]
        mkEmpty outcome showedVal t = makeFullTree outcome descr showedVal [t]
        fTree = m Positive Nothing

-- | Checks that elements of the container are matched by the matchers
-- exactly. The number of elements in the container should match the
-- number of matchers in the list.
elementsAre :: (Foldable t, Monad f, Show (t a))
            => [MatcherF f a] -- ^ List of matchers for the elements of the list.
            -> MatcherF f (t a) -- ^ Matcher for the whole container.
elementsAre matcherList dir = maybe emptyTree mkTree
  where
    emptyTree = makeEmptyTree descr <$> sequenceA (map (\m -> m Positive Nothing) matcherList)

    mkTree fitems = do
      items <- fitems
      subnodes <- sequenceA $ go (toList items) matcherList 0
      return $ makeFullTree (applyDirection dir (all mtValue subnodes)) name items subnodes

    numMatchers = length matcherList
    name  = hsep ["container", "such", "that"]
    name_ = hsep ["container", "such", "that", "not", "all", "of"]
    descr = pickDescription dir (name, name_)

    sizeMessage  = hsep ["number", "of", "elements", "is", display numMatchers]

    go (x:xs) (m:ms) n = m Positive (Just $ pure x) : go xs ms (n + 1)
    go [] [] n = [pure $ makeFullTree True sizeMessage n []]
    go moreItems@(_:_) [] n = [pure $ makeFullTree False sizeMessage (n + length moreItems) []]
    go [] (m:_) n = [ m Positive Nothing
                    , pure $ makeFullTree False sizeMessage n []
                    ]

-- | Matcher that succeeds if the argument starts with the specified
-- prefix.
startsWith :: (Applicative f, Show a, Eq a)
           => [a]            -- ^ The prefix the list is expected to have.
           -> MatcherF f [a]
startsWith xs = predicate (xs `isPrefixOf`)
                ( hsep ["starts", "with", display xs]
                , hsep ["does", "not", "start", "with", display xs]
                )

-- | Matcher that succeeds if the argument ends with the specified
-- suffix.
endsWith :: (Applicative f, Show a, Eq a)
           => [a]            -- ^ The suffix the list is expected to have.
           -> MatcherF f [a]
endsWith xs = predicate (xs `isSuffixOf`)
              ( hsep ["ends", "with", display xs]
              , hsep ["does", "not", "end", "with", display xs]
              )

-- | Matcher that succeeds if the argument contains the given infix.
hasInfix :: (Applicative f, Show a, Eq a)
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
tuple2 mx my = projection "fst" fst mx `andAlso` projection "snd" snd my

-- | Builds a matcher for a 3-tuple from the matchers for components.
tuple3
  :: (Show a, Show b, Show c, Applicative f)
  => MatcherF f a -- ^ Matcher for the 1st element of the 3-tuple.
  -> MatcherF f b -- ^ Matcher for the 2nd element of the 3-tuple.
  -> MatcherF f c -- ^ Matcher for the 3rd element of the 3-tuple.
  -> MatcherF f (a, b, c)
tuple3 mx my mz = allOf [ projection "#1" (\(x,_,_) -> x) mx
                        , projection "#2" (\(_,y,_) -> y) my
                        , projection "#3" (\(_,_,z) -> z) mz
                        ]

-- | A matcher for 'Maybe' that matches only 'Nothing'.
isNothing
  :: (Show a, Applicative f)
  => MatcherF f (Maybe a)
isNothing = predicate (\m -> case m of Nothing -> True; _ -> False)
            ("is Nothing", "is not Nothing")

-- | Builds a matcher for 'Maybe' that only matches 'Just' values
-- satisfying the given matcher.
isJustWith
  :: (Show a, Applicative f, Traversable f)
  => MatcherF f a
  -> MatcherF f (Maybe a)
isJustWith = prism "Just" id

-- | Makes a matcher that only matches 'Left' values satisfying given
-- matcher.
isLeftWith
  :: (Show a, Show b, Traversable f, Applicative f)
  => MatcherF f a -- ^ Matcher for the left side of 'Either'.
  -> MatcherF f (Either a b)
isLeftWith  = prism "Left"  $ \x -> case x of { Left a  -> Just a; _ -> Nothing }

-- | Makes a matcher that only matches 'Right' values satisfying given
-- matcher.
isRightWith
  :: (Show a, Show b, Traversable f, Applicative f)
  => MatcherF f b -- ^ Matcher for the right side of 'Either'.
  -> MatcherF f (Either a b)
isRightWith = prism "Right" $ \x -> case x of { Right b -> Just b; _ -> Nothing }

-- | Implementation of Data.Functor.Covariant.contramap.
-- Avoid using it directly, prefer 'projection' instead.
contramap :: (Functor f) => (s -> a) -> MatcherF f a -> MatcherF f s
contramap f p dir = p dir . fmap (fmap f)

-- | Implementation of Data.Functor.Covariant.contramap for a matcher set.
contramapSet
  :: (Functor f)
  => (s -> a)
  -> MatcherSetF f a
  -> MatcherSetF f s
contramapSet f set = MatcherSetF run
  where run dir = matchSetF set dir . fmap (fmap f)

-- | Makes a matcher that attaches a label to the outcome of another
-- matcher.
labeled
  :: (Functor f)
  => String
  -> MatcherF f a
  -> MatcherF f a
labeled l = transformTree addLabel
  where addLabel t = t { mtLabels = l : mtLabels t }

-- | Builds a matcher for a structure from a matcher of its substructure.
projection
  :: (Show s, Applicative f)
  => String -- ^ The name of the projection of the structure 's'
  -> (s -> a) -- ^ The projection from a structure 's' to it's substructure 'a'.
  -> MatcherF f a -- ^ Matcher of the substructure 'a'.
  -> MatcherF f s
projection name proj m = projectionWithSet name proj (matcher m)

-- | Builds a matcher for a structure from a set of matchers for its substructure.
-- The whole matcher succeeds if all the matchers from the set succeed.
--
-- prop> x `matches` (projectionWithSet "p" p set) ⇔ x `matches` (projection "p" p (allOfSet set))
-- prop> x `matches` (projection "p" p m) ⇔ x `matches` (projectionWithSet "p" p (matcher m))
projectionWithSet
  :: (Show s, Applicative f)
  => String -- ^ The name of the projection.
  -> (s -> a) -- ^ The projection from "s" to "a".
  -> MatcherSetF f a -- ^ The set of matchers for the projected "a".
  -> MatcherF f s
projectionWithSet name proj set dir value =
  let subnodesF = matchSetF (contramapSet proj set) dir value
      descr  = hsep ["projection", symbol name]
      msgF = sequenceA $ fmap (fmap show) value
  in liftA2 (\xs msg -> MatchTree (all mtValue xs) descr [] msg xs)
     subnodesF msgF

-- | Builds a matcher for one alternative of a sum type given matcher for a
-- prism projection.
prism
  :: (Show s, Traversable f, Applicative f)
  => String -- ^ The name of the selected alternative of the structure "s".
  -> (s -> Maybe a) -- ^ The selector for the alternative "a".
  -> MatcherF f a -- ^ The matcher for the value of the alternative.
  -> MatcherF f s
prism name p m = prismWithSet name p (matcher m)

-- | A version of 'prism' that takes a set of matchers for the value
-- instead of a single one. The results of the matchers in the set are
-- aggregated with 'and'. This results in a shallower and less
-- cluttered trees when defining custom matchers comparing to 'prism'
-- followed by 'allOf'.
--
-- prop> x `matches` (prismWithSet "p" p set) ⇔ x `matches` (prism "p" p (allOfSet set))
-- prop> x `matches` (prism "p" p m) ⇔ x `matches` (prismWithSet "p" p (matcher m))
prismWithSet
  :: (Show s, Traversable f, Applicative f)
  => String -- ^ The name of the selected alternative.
  -> (s -> Maybe a) -- ^ The selector for the alternative "a".
  -> MatcherSetF f a -- ^ The set of matchers for the alternative "a".
  -> MatcherF f s
prismWithSet name p set dir v =
  case v of
    Nothing -> makeEmptyTree descr <$> matchSetF set dir Nothing
    Just fs -> (\subtrees s -> makeFullTree (all mtValue subtrees) descr s subtrees)
      <$> matchSetF set dir (sequenceA $ fmap p fs)
      <*> fs
  where descr = hsep ["prism", symbol name]


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
throws exMatcher dir maybeAction = do
  let excName = symbol $ typeOf (undefined :: e)
      descr  = hsep ["action", "throwing", excName, "that"]
      descr_  = hsep ["action", "not", "throwing", excName, "that"]
      d = pickDescription dir (descr, descr_)
      
  case maybeAction of
    Nothing ->
      return $ makeEmptyTree d [runIdentity $ exMatcher Positive Nothing]
    Just action -> do
      outcome <- tryExn action
      case outcome of
        NoExn -> return
          $ makeEmptyTree d [runIdentity $ exMatcher Positive Nothing]
        ExpectedExn exn -> return $ let node = match exn exMatcher
                                    in makeFullTree (applyDirection dir (mtValue node)) d exn [node]
        OtherExn exn -> return $ makeFullTree (applyDirection dir False) d exn
                                 [runIdentity $ exMatcher Positive Nothing]

-- | Runs a matcher on the given effectful value and returns the
-- `MatchTree` wrapped into the same effect.
runMatcher
  :: MatcherF f a -- ^ The matcher to run.
  -> f a -- ^ The effectful value.
  -> f MatchTree -- ^ The resulting match tree.
runMatcher m value = m Positive (Just value)

-- | Runs a pure matcher on the given value and returns the
-- 'MatchTree'.
match
  :: a -- ^ Value to match.
  -> Matcher a -- ^ Matcher to run on the value.
  -> MatchTree
match x p = runIdentity $ runMatcher p (Identity x)

-- | Returns 'True' if the given value matches the provided pure
-- matcher, otherwise returns 'False'.
matches
  :: a -- ^ Value to match.
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
ma &> mb = MatcherSetF $ \dir maybeP ->
  liftA2 mappend
  (matchSetF ma dir $ fmap (fmap fst) maybeP)
  (matchSetF mb dir $ fmap (fmap snd) maybeP)

infixl 7 &>
