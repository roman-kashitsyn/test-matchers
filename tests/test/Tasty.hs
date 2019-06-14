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
import Test.Matchers
  ( MatcherF
  , allOf
  , andAlso
  , anything
  , contramapSet
  , eq
  , labeled
  , projection
  , (&>)
  )
import Test.Matchers.Tasty (shouldMatch, shouldNotMatch, testCase)
import Test.Tasty (TestTree, defaultMain, testGroup)

data Name
  = Name
    { firstName :: String
    , secondName :: String
    } deriving (Eq, Show)
n
data User
  = User
    { userId :: Int
    , userName :: Name
    } deriving (Eq, Show)

userNameIs :: (Monad f) => MatcherF f String -> MatcherF f String -> MatcherF f Name
userNameIs fstNameM sndNameM = allOf $
                               contramapSet mkTuple (labeled "firstName" fstNameM
                                                     &> labeled "secondName" sndNameM)
  where mkTuple (Name f s) = (f, s)

hasUserName :: (Monad f) => MatcherF f Name -> MatcherF f User
hasUserName = projection "userName" userName

hasUserId :: (Monad f) => MatcherF f Int -> MatcherF f User
hasUserId = projection "userId" userId

isUserWith
  :: (Monad f)
  => MatcherF f Int
  -> MatcherF f Name
  -> MatcherF f User
isUserWith idMatcher nameMatcher
  = hasUserId idMatcher `andAlso` hasUserName nameMatcher

tests :: TestTree
tests = let user = User 5 (Name "John" "Smith")
        in testGroup "Test.Matchers.Tasty"
           [ testCase "Test user name" $
             user `shouldMatch` hasUserName (userNameIs (eq "John") anything)
           , testCase "Test user id" $
             user `shouldMatch` hasUserId (eq 5)
           , testCase "Test negative match" $
             user `shouldNotMatch` isUserWith (eq 1) (userNameIs (eq "Fred") anything)
           ]

main :: IO ()
main = defaultMain tests
