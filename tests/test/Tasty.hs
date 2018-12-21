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
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Matchers.Tasty (testCase, shouldMatch, shouldNotMatch)
import Test.Matchers
  ( MatcherF
  , andAlso
  , eq
  , projection
  )

data User
  = User
    { userId :: Int
    , userName :: String
    } deriving (Eq, Show)

hasUserName :: (Monad f) => MatcherF f String -> MatcherF f User
hasUserName = projection "userName" userName

hasUserId :: (Monad f) => MatcherF f Int -> MatcherF f User
hasUserId = projection "userId" userId

isUserWith
  :: (Monad f)
  => MatcherF f Int
  -> MatcherF f String
  -> MatcherF f User
isUserWith idMatcher nameMatcher
  = hasUserId idMatcher `andAlso` hasUserName nameMatcher

tests :: TestTree
tests = let user = User 5 "name"
        in testGroup "Test.Matchers.Tasty"
           [ testCase "Test user name" $  user `shouldMatch` hasUserName (eq "name")
           , testCase "Test user id" $ user `shouldMatch` hasUserId (eq 5)
           , testCase "Test negative match" $ user `shouldNotMatch` isUserWith (eq 1) (eq "name")
           ]

main :: IO ()
main = defaultMain tests
