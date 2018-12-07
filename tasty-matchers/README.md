# Tasty Matchers

This package enables one to use `test-matchers` together with the amazing
[tasty](http://hackage.haskell.org/package/tasty) library.

Please prefer this integration path to combination `tasty` + `tasty-hunit` +
`test-matchers` + `hunit-matchers` (which is also a possibility, but
`tasty-matchers` provides a better integration with `tasty`).

# Example

Make sure you've added dependencies on `tasty`, `test-matchers` and
`tasty-matchers` to the corresponding test stanza in your `.cabal` file.

```haskell
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Matchers.Tasty (testCase, shouldMatch, shouldNotMatch)
import Test.Matchers
  ( MatcherF
  , allOf
  , eq
  , projection
  )

data User
  = User
    { userId :: Int
    , userName :: String
    } deriving (Eq, Show)

hasUserName :: (Monad f) => MatcherF f String -> MatcherF f User
hasUserName matchName = projection "userName" userName matchName

hasUserId :: (Monad f) => MatcherF f Int -> MatcherF f User
hasUserId matchId = projection "userId" userId matchId

isUserWith
  :: (Monad f)
  => MatcherF f Int
  -> MatcherF f String
  -> MatcherF f User
isUserWith idMatcher nameMatcher
    = allOf [hasUserId idMatcher, hasUserName nameMatcher]

tests :: TestTree
tests = let user = User 5 "name"
        in testGroup "Test my user"
           [ testCase "Test user name" $  user `shouldMatch` hasUserName (eq "name")
           , testCase "Test user id" $ user `shouldMatch` hasUserId (eq 5)
           , testCase "Test negative match" $ user `shouldNotMatch` isUserWith (eq 1) (eq "name")
           ]

main :: IO ()
main = defaultMain tests
```

# Command-line options

When you use `tasty-matchers` to write your tests, the test binaries
built with `tasty` provide the following options controlling the
appearance of the error messages:

<table>
<thead>
  <tr>
    <th>Option name</th>
    <th>Possible values</th>
    <th>Effect</th>
  </tr>
</thead>
<tbody>
  <tr>
    <td>color</td>
    <td><ul><li>never<li>always<li>auto</ul></td>
    <td>When to use colors and formatting (bold, italics) in error messages</td>
  </tr>
  <tr>
    <td>unicode</td>
    <td><ul><li>never<li>always<li>auto</ul></td>
    <td>When to use fancy unicode symbols inside of error messages</td>
  </tr>
</tbody>
</table>
