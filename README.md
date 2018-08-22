# Test.Matchers

A simple matcher combinators library for unit-testing. The main point
of the library is to make the error messages produced by test failure
as clear and useful as possible. For example, the following expression:

```haskell
type E = Either String Int

(Left "The length argument is out of bounds" :: E, Right 5 :: E)
  `shouldMatch`
  tuple2 (leftIs $ containsInOrder "size") (rightIs $ gt 0)
```

produces the following error message:

```
☒ all of ← (Left "The length argument is out of bounds",Right 5)
  ☒ property fst is ← (Left "The length argument is out of bounds",Right 5)
    ☒ prism Left is ← Left "The length argument is out of bounds"
      ☒ contains in order "size" ← "The length argument is out of bounds"
  ☑ property snd is ← (Left "The length argument is out of bounds",Right 5)
    ☑ prism Right is ← Right 5
      ☑ a value > 0 ← 5
```

The whole comparison is represented as a tree, and it's pretty easy to
spot paths which led to the test failure and observe the values
expected and received by individual predicates.
