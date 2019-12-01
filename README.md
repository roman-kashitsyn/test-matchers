# Test.Matchers — composable assertions with decent error messages

A simple matcher combinators library for unit-testing heavily inspired by [GMock Matchers][1]

## Why do I need it?

  * It provides a simple language to express and combine assertions.
    Always feel like `Test.Hspec.Expectations` lacks that simple function you need?
    You're not alone.

  * It produces detailed yet compact (and colorful) error messages in case of an assertion failure.

  * It highlights syntax within values it displays.
    All you need is a `Show` instance.

  * It easily integrates with test frameworks capable of executing HUnit assertions (e.g., [HUnit][2] or [hspec][3]), [tasty][4] and [QuickCheck][5].
    These integrations are provided by `hunit-matchers`, `tasty-matchers` and `quickcheck-matchers` packages.

## Short example

```haskell
import Test.HUnit
import Test.Matchers
import Test.Matchers.HUnit

myDiv :: Int -> Int -> Either String Int
myDiv _ 0 = Left "Division by zero"
myDiv x y = Right (x `quot` y)

good = TestCase $ myDiv 5 0 `shouldMatch` (isLeftWith $ hasInfix "zero")
bad  = TestCase $ myDiv 5 0 `shouldMatch` (isRightWith $ eq 0)

main = rutTestTT $ TestList [good, bad]
```

The failed test in the example above displays the following error message:

<pre><span>✘ prism "Right"</span> ← <1>
  <span>✘ is a value equal to <em>0</em></span>
where:
  <1> <em>Left "Division by zero"</em></pre>

Matchers are not limited to pure values, they can also match the outcome of an `IO` operations.
An example of checking that an `IO` action throws the exception you expect:

```haskell
myAction `shouldMatchIO`
  --
  --  +- 'myAction' must throw an 'IOException' (the type is deduced from 'ioe_type' in 'projection')
  --  v
  (throws $ projection "ioe_type" ioe_type [eq UnsupportedOperation])
  --        ^                                  ^
  --        +- such that applying 'ioe_type'   |
  --                                           +- results in 'UnsupportedOperation'
```

# Related resources

There is a talk describing the motivation behind the library and some design choices made ([video][6], [slides][7]).

# Disclaimer

This is not an official Google product.

[1]: https://github.com/google/googletest/blob/master/googlemock/docs/CheatSheet.md#matchers
[2]: https://github.com/hspec/HUnit#readme
[3]: https://hspec.github.io
[4]: https://github.com/feuerbach/tasty
[5]: http://hackage.haskell.org/package/QuickCheck
[6]: https://www.youtube.com/watch?v=6F_KYfe442Y
[7]: https://github.com/meiersi/HaskellerZ/blob/master/meetups/2018-10-25-embracing-the-failure/Slides.pdf
