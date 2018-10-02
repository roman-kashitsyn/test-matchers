# Test.Matchers

A simple matcher combinators library for unit-testing heavily inspired
by [GMock
Matchers](https://github.com/google/googletest/blob/master/googlemock/docs/CheatSheet.md#matchers).
The main purpose of the library is to make the error messages produced
by test failures clearer and more useful.

It easily integrates with any test framework capable of executing
HUnit assertions (e.g. [hspec](https://hspec.github.io) or
[tasty](https://github.com/feuerbach/tasty)).

Short example:

```haskell
div :: Int -> Int -> Either String Int
div _ 0 = Left "Division by zero"
div x y = Right (x `quot` y)

-- This test passes
div 5 0 `shouldMatch` (leftIs $ hasInfix "zero")

-- This test fails
div 5 0 `shouldMatch` (rightIs $ eq 0)
```

The failed test display the following error message:

<pre><span>✘ prism Right is</span> ← <em>Left "Division by zero"</em>
  <span>✘ a value equal to <em>0</em></span> ← nothing</pre>

You can also match exceptions:

```haskell
myAction `shouldMatchIO`
  (throws $ property "ioe_type" ioe_type $ eq UnsupportedOperation)
```

# Disclaimer

This is not an official Google product.
