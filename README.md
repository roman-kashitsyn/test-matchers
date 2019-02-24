# Test.Matchers

A simple matcher combinators library for unit-testing heavily inspired
by [GMock
Matchers](https://github.com/google/googletest/blob/master/googlemock/docs/CheatSheet.md#matchers).

## Why do I need it?

  * It provides a simple language to express and combine assertions.
    Always feel like `Test.Hspec.Expectations` lacks the trivial
    function you need? Fear no more.

  * It produces detailed yet compact (and colorful) error messages in
    case of a test failure.

  * It highlights syntax within values it displays. All you need is
    a Show instance. An un-readable one will work too. And yes, your
    value can be infinite. This code won't eat all your RAM[^1]:
    ```haskell
    repeat 5 `shouldMatch` startsWith [6]
    ```

  * It easily integrates with many test frameworks capable of
    executing HUnit assertions
    (e.g. [HUnit](https://github.com/hspec/HUnit#readme) or
    [hspec](https://hspec.github.io)) and
    [tasty](https://github.com/feuerbach/tasty). These integrations
    are provided by `hunit-matchers` and `tasty-matchers` packages.

[^1]: Though this code can:
    ```haskell
    repeat 5 `shouldMatch` eq [5, 5..]
    ```

## Short example

```haskell
myDiv :: Int -> Int -> Either String Int
myDiv _ 0 = Left "Division by zero"
myDiv x y = Right (x `quot` y)

-- This test passes
myDiv 5 0 `shouldMatch` (leftIs $ hasInfix "zero")

-- This test fails
myDiv 5 0 `shouldMatch` (rightIs $ eq 0)
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
# Related resources

There is a talk describing the motivation behind the library and some
design choices made
([video](https://www.youtube.com/watch?v=6F_KYfe442Y),
[slides](https://github.com/meiersi/HaskellerZ/blob/master/meetups/2018-10-25-embracing-the-failure/Slides.pdf)).

# Disclaimer

This is not an official Google product.
