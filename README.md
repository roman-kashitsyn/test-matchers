# Test.Matchers

A simple matcher combinators library for unit-testing heavily inspired
by [GMock
Matchers](https://github.com/google/googletest/blob/master/googlemock/docs/CheatSheet.md#matchers).
The main purpose of the library is to make the error messages produced
by test failures clearer and more useful.

It easily integrates with any test framework capable of executing
HUnit assertions (e.g. [hspec](https://hspec.github.io) or
[tasty](https://github.com/feuerbach/tasty)).

## Short example

```haskell
div :: Int -> Int -> Either String Int
div _ 0 = Left "Division by zero"
div x y = Right (x `quot` y)

-- This test passes
div 5 0 `shouldMatch` (leftIs $ hasInfix "zero")

-- This test fails
div 5 0 `shouldMatch` (rightIs $ eq 0)
```

The failed test in the example above displays the following error
message:

<pre><span>✘ prism Right</span> ← <1>
  <span>✘ is a value equal to <em>0</em></span>
where:
  <1> <em>Left "Division by zero"</em></pre>

You can also match exceptions:

```haskell
myAction `shouldMatchIO`
  (throws $ projection "ioe_type" ioe_type $ eq UnsupportedOperation)
```

## Integration with `Control.Lens`

You can easily turn your lenses and prisms into matchers using the
`projection` and `prism` matcher combinators:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Test.Matchers (MatcherF)
import qualified Test.Matchers as M

data Tree a
   = Leaf a
   | Fork (Tree a) a (Tree a)
   deriving (Show, Eq)

makePrisms ''Tree

-- A lens for the root of the tree
root :: Lens' (Tree a) a
root f (Leaf x) = fmap Leaf (f x)
root f (Fork l x r) = fmap (\x' -> Fork l x' r) (f x)

-- Generated by 'makePrisms'
_Leaf :: Prism' (Tree a) a
_Fork :: Prism' (Tree a) (Tree a, a, Tree a)

-- A matcher reusing the 'root' lens.
rootWith :: (Show a, Monad f) => MatcherF f a -> MatcherF f (Tree a)
rootWith matchRoot = M.projection "root" (view root) matchRoot

-- A matcher reusing the '_Leaf' prism.
isLeafWith :: (Show a, Monad f) => MatcherF f a -> MatcherF f (Tree a)
isLeafWith matchA = M.prism "Leaf" (preview _Leaf) matchA

-- A matcher reusing the '_Fork' prism.
isForkWith
  :: (Show a, Monad f)
  => MatcherF f (Tree a)
  -> MatcherF f a
  -> MatcherF f (Tree a)
  -> MatcherF f (Tree a)
isForkWith matchL matchA matchR
  = M.prism "Fork" (preview _Fork) (M.tuple3 matchLeft matchA matchR)
```

# Disclaimer

This is not an official Google product.
