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
import Test.Hspec

import Data.String (IsString(..))
import Data.List (intercalate)
import GHC.IO.Exception
import Control.Exception
import Test.Matchers
import Test.HUnit.Lang (HUnitFailure(..), FailureReason(Reason))
import System.Environment (unsetEnv, setEnv)

data Tree a = Leaf a | Fork (Tree a) (Tree a) deriving (Show, Eq)

isLeafWith
  :: (Show a, Applicative f, Traversable f)
  => MatcherF f a
  -> MatcherF f (Tree a)
isLeafWith = prism "Leaf" (\t -> case t of Leaf x' -> Just x'; _ -> Nothing)

isForkWith
  :: (Show a, Applicative f, Traversable f)
  => MatcherF f (Tree a)
  -> MatcherF f (Tree a)
  -> MatcherF f (Tree a)
isForkWith leftM rightM = prism "Fork"
  (\t -> case t of Fork l' r' -> Just (l' &. r'); _ -> Nothing)
  (allOfSet $ matcher leftM &> matcher rightM)

treeEq
  :: (Show a, Eq a, Applicative f, Traversable f)
  => Tree a
  -> MatcherF f (Tree a)
treeEq (Leaf x) = isLeafWith (eq x)
treeEq (Fork l r) = isForkWith (treeEq l) (treeEq r)

ok :: Message -> Message -> MatchTree
ok msg val = MatchTree True msg (Just val) []

ok' :: Message -> MatchTree
ok' msg = MatchTree True msg Nothing []

nok :: Message -> Message -> MatchTree
nok msg val = MatchTree False msg (Just val) []

nok' :: Message -> MatchTree
nok' msg = MatchTree False msg Nothing []

colorVar :: String
colorVar = "TEST_MATCHERS_COLOR"

withNoColor :: SpecWith a -> SpecWith a
withNoColor = around_ $ bracket_ (setEnv colorVar "0") (unsetEnv colorVar)

failureMessageIs :: Expectation -> String -> Expectation
failureMessageIs test msg =
  test `shouldThrow` (\(HUnitFailure _ reason) -> reason == Reason msg)

type T = Either String Int

main :: IO ()
main = hspec $ do
  describe "Simple matchers" $ do

    it "can match for equality" $ do
      match 5 (eq 5) `shouldBe` ok "is a value equal to 5" "5"
      match 0 (eq 1) `shouldBe` nok "is a value equal to 1" "0"
      match 0 (ne 1) `shouldBe` ok "is a value not equal to 1" "0"

    it "can match comparable types" $ do
      match 3 (gt 1) `shouldBe` ok "is a value > 1" "3"
      match 1 (ge 1) `shouldBe` ok "is a value ≥ 1" "1"
      match 3 (ge 1) `shouldBe` ok "is a value ≥ 1" "3"

      match 3 (lt 4) `shouldBe` ok "is a value < 4" "3"
      match 3 (le 4) `shouldBe` ok "is a value ≤ 4" "3"
      match 3 (le 3) `shouldBe` ok "is a value ≤ 3" "3"

      match 3 (gt 4) `shouldBe` nok "is a value > 4" "3"
      match 3 (ge 4) `shouldBe` nok "is a value ≥ 4" "3"

      match 3 (lt 1) `shouldBe` nok "is a value < 1" "3"
      match 3 (le 1) `shouldBe` nok "is a value ≤ 1" "3"

    it "can approx. match floats" $ do
      let almost1 = (sum $ replicate 10 0.1) :: Double
      almost1 `shouldNotBe` 1.0
      almost1 `shouldMatch` floatApproxEq 1.0
      almost1 `shouldNotMatch` floatApproxEq 1.0000000000001
      1.0 `shouldMatch` floatApproxEq almost1
      (- almost1) `shouldMatch` floatApproxEq (-1.0)
      (- almost1) `shouldNotMatch` floatApproxEq 1.0

    it "can match near floats" $ do
      0.00005 `shouldMatch` numberNear 0.00005 0
      0.000005 `shouldMatch` numberNear 0.00005 0
      0.00006 `shouldNotMatch` numberNear 0.00005 0
      0.1 `shouldNotMatch` numberNear 0.00005 0

    it "can match pairs" $ do
      match (3, 4) (tuple2 (eq 3) (gt 1)) `shouldBe`
        MatchTree True "all of" (Just "(3,4)")
        [ MatchTree True "projection \"fst\"" (Just "(3,4)")
          [ok "is a value equal to 3" "3"]
        , MatchTree True "projection \"snd\"" (Just "(3,4)")
          [ok "is a value > 1" "4"]
        ]

    it "can match Either a b" $ do
      match (Left 3 :: Either Int String) (isLeftWith (eq 3)) `shouldBe`
        MatchTree True
        "prism \"Left\""
        (Just "Left 3") [ok "is a value equal to 3" "3"]
      match (Right "ok" :: Either Int String) (isRightWith anything) `shouldBe`
        MatchTree True
        "prism \"Right\""
        (Just "Right \"ok\"") [ok "anything" "\"ok\""]

    it "can match list prefixes/suffixes/infixes" $ do
      match [1, 1, 2, 3] (startsWith [1, 1]) `shouldBe`
        ok "starts with [1,1]" "[1,1,2,3]"
      match [1, 1, 2, 3] (endsWith [2, 3]) `shouldBe`
        ok "ends with [2,3]" "[1,1,2,3]"
      match [1, 1, 2, 3] (hasInfix [1, 2]) `shouldBe`
        ok "has infix [1,2]" "[1,1,2,3]"

    it "can match lists" $ do
      match ([] :: [Int])  (elementsAre []) `shouldBe`
        MatchTree True "container such that" (Just "[]")
        [ok "number of elements is 0" "0"]

      match [] (elementsAre [eq 5]) `shouldBe`
        MatchTree False "container such that" (Just "[]")
        [ nok' "is a value equal to 5"
        , nok "number of elements is 1" "0"
        ]
      match [5, 7] (elementsAre [eq 5]) `shouldBe`
        MatchTree False "container such that" (Just "[5,7]")
        [ ok "is a value equal to 5" "5"
        , nok "number of elements is 1" "2"
        ]
      match [5, 7] (elementsAre [eq 5, gt 5]) `shouldBe`
        MatchTree True "container such that" (Just "[5,7]")
        [ ok "is a value equal to 5" "5"
        , ok "is a value > 5" "7"
        , ok "number of elements is 2" "2"
        ]

    it "can match each element of a list" $ do
      match [1,2] (each $ gt 0) `shouldBe`
        MatchTree True "each element of the container" (Just "[1,2]")
        [ ok "is a value > 0" "1"
        , ok "is a value > 0" "2"
        ]

      match [-1,1] (each $ gt 0) `shouldBe`
        MatchTree False "each element of the container" (Just "[-1,1]")
        [ nok "is a value > 0" "-1"
        , ok "is a value > 0" "1"
        ]

      match [] (each $ gt 0) `shouldBe`
        MatchTree True "each element of the container" (Just "[]")
        [ nok' "is a value > 0" ]

      match [] (negationOf $ each $ gt 0) `shouldBe`
        MatchTree False "container has at least one element that" (Just "[]")
        [ nok' "is a value > 0" ]

      match [0,5] (negationOf $ each $ lt 5) `shouldBe`
        MatchTree True "container has at least one element that" (Just "[0,5]")
        [ ok "is a value < 5" "0"
        , nok "is a value < 5" "5"
        ]

    it "can aggregate matchers" $ do
      match 1 (allOf [gt 0, lt 5]) `shouldBe`
        MatchTree True "all of" (Just "1")
        [ ok "is a value > 0" "1"
        , ok "is a value < 5" "1"
        ]
      match 1 (allOf [gt 1, lt 5]) `shouldBe`
        MatchTree False "all of" (Just "1")
        [ nok "is a value > 1" "1"
        , ok "is a value < 5" "1"
        ]
      match 1 (oneOf [lt 1, ne 0]) `shouldBe`
        MatchTree True "one of" (Just "1")
        [ nok "is a value < 1" "1"
        , ok "is a value not equal to 0" "1"
        ]
      match 1 (oneOf [lt (-1), gt 1]) `shouldBe`
        MatchTree False "one of" (Just "1")
        [ nok "is a value < -1" "1"
        , nok "is a value > 1" "1"
        ]
  
  describe "Container matching" $ do
    it "can check if container is empty" $ do
      match (Nothing :: Maybe Int) isEmpty `shouldBe` ok "is empty" "Nothing"
      match ([] :: [Int]) isEmpty `shouldBe` ok "is empty" "[]"
      match (Just 1) isEmpty `shouldBe` nok "is empty" "Just 1"
      match [1,2] isEmpty `shouldBe` nok "is empty" "[1,2]"
      match [1] isNotEmpty `shouldBe` ok "is not empty" "[1]"
      match ([] :: [Int]) isNotEmpty `shouldBe` nok "is not empty" "[]"

    it "can check the length of a container" $ do
      match (Just 1) (hasLength $ gt 0) `shouldBe`
        MatchTree True "projection \"length\"" (Just "Just 1")
        [ ok "is a value > 0" "1" ]
      match [1,2,3] (hasLength $ eq 4) `shouldBe`
        MatchTree False "projection \"length\"" (Just "[1,2,3]")
        [ nok "is a value equal to 4" "3" ]

  describe "Test.HUnit integration" $ do

    it "can use `shouldMatch` for assertions" $ do
      ("a" :: String, 5) `shouldMatch` (tuple2 (elementsAre [eq 'a']) (gt 0))

    it "can run `shouldMatchIO` for monad assertions" $ do
      (pure 5) `shouldMatchIO` (gt 0)

  describe "Tree printing" $ withNoColor $ do
    it "prints trees with forward references" $ do
      let input = "a very long string that should be turn into a ref" :: String
      (input `shouldMatch` allOf [isEmpty, isNotEmpty]) `failureMessageIs`
        intercalate "\n"
        [ "✘ all of ← <1>"
        , "  ✘ is empty ← <1>"
        , "  ✔ is not empty ← <1>"
        , "where:"
        , "  <1> " ++ show input
        ]
  
  describe "Exception matching" $ do

    it "can match exceptions as normal values" $ do
      (ioError unsupportedOperation) `shouldMatchIO`
        (throws $ projection "ioe_type" ioe_type (eq UnsupportedOperation))

    it "can match any exception" $ do
      ioError unsupportedOperation `shouldMatchIO`
        throws (anything :: Matcher SomeException)

    it "does not rethrow unmatched exception" $ do
      (throws (eq DivideByZero) `runMatcher` ioError unsupportedOperation) `shouldReturn`
        (MatchTree False
         "action throwing ArithException that"
         (Just $ fromString $ show unsupportedOperation)
         [nok' (fromString $ "is a value equal to " ++ show DivideByZero)
         ])

  describe "README examples" $ withNoColor $ do
    let div :: Int -> Int -> Either String Int
        div _ 0 = Left "Division by zero"
        div x y = Right (x `quot` y)
    it "works for positive case" $ do
      div 5 0 `shouldMatch` (isLeftWith $ hasInfix "zero")
    it "works for negative case" $ do
      div 5 0 `shouldMatch` (isRightWith $ eq 0)
        `failureMessageIs`
        (intercalate "\n" [ "✘ prism \"Right\" ← <1>"
                          , "  ✘ is a value equal to 0"
                          , "where:"
                          , "  <1> Left \"Division by zero\""
                          ])

  describe "Custom matchers" $ do
    it "can match trees" $ do
      let t = (Fork (Fork (Leaf 5) (Leaf 7)) (Leaf 10))
      t `shouldMatch` treeEq t
