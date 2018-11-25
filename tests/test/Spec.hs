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
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Hspec

import Data.String (IsString(..))
import Data.List (intercalate)
import Data.Functor.Identity (Identity (..), runIdentity)
import GHC.IO.Exception
import Control.Exception
import Test.Matchers
import Test.Matchers.HUnit
import Test.QuickCheck (forAll, elements, property, counterexample)
import qualified Test.Matchers.HUnit.Implicit as I
import Test.HUnit.Lang (HUnitFailure(..), FailureReason(Reason))

default (Integer, Double)

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
isForkWith leftM rightM = prismWithSet "Fork" p (matcher leftM &> matcher rightM)
  where p (Fork l r) = Just (l &. r)
        p _ = Nothing

treeEq
  :: (Show a, Eq a, Applicative f, Traversable f)
  => Tree a
  -> MatcherF f (Tree a)
treeEq (Leaf x) = isLeafWith (eq x)
treeEq (Fork l r) = isForkWith (treeEq l) (treeEq r)

ok :: Message -> Message -> MatchTree
ok msg val = MatchTree True msg (Just val) []

nok :: Message -> Message -> MatchTree
nok msg val = MatchTree False msg (Just val) []

nok' :: Message -> MatchTree
nok' msg = MatchTree False msg Nothing []

failureMessageIs :: Expectation -> String -> Expectation
failureMessageIs test msg =
  test `shouldThrow` (\(HUnitFailure _ reason) -> reason == Reason msg)

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

    it "can match tuples" $ do
      match (3, 4) (tuple2 (eq 3) (gt 1)) `shouldBe`
        MatchTree True "all of" (Just "(3,4)")
        [ MatchTree True "projection \"fst\"" (Just "(3,4)")
          [ok "is a value equal to 3" "3"]
        , MatchTree True "projection \"snd\"" (Just "(3,4)")
          [ok "is a value > 1" "4"]
        ]

      match (3, 4, 5) (tuple3 (eq 3) (gt 1) (gt 3)) `shouldBe`
        MatchTree True "all of" (Just "(3,4,5)")
        [ MatchTree True "projection \"#1\"" (Just "(3,4,5)")
          [ok "is a value equal to 3" "3"]
        , MatchTree True "projection \"#2\"" (Just "(3,4,5)")
          [ok "is a value > 1" "4"]
        , MatchTree True "projection \"#3\"" (Just "(3,4,5)")
          [ok "is a value > 3" "5"]
        ]
      match (3, 4, 5) (tuple3 (lt 4) (lt 4) (lt 4)) `shouldBe`
        MatchTree False "all of" (Just "(3,4,5)")
        [ MatchTree True "projection \"#1\"" (Just "(3,4,5)")
          [ok "is a value < 4" "3"]
        , MatchTree False "projection \"#2\"" (Just "(3,4,5)")
          [nok "is a value < 4" "4"]
        , MatchTree False "projection \"#3\"" (Just "(3,4,5)")
          [nok "is a value < 4" "5"]
        ]

    it "can match Maybe a" $ do
      match (Nothing :: Maybe Int) isNothing `shouldBe`
        ok "is Nothing" "Nothing"
      match (Just 5) isNothing `shouldBe`
        nok "is Nothing" "Just 5"
      match (Nothing :: Maybe Int) (negationOf isNothing) `shouldBe`
        nok "is not Nothing" "Nothing"
      match (Just 5) (negationOf isNothing) `shouldBe`
        ok "is not Nothing" "Just 5"

      match (Nothing :: Maybe Int) (isJustWith $ eq 5) `shouldBe`
        MatchTree False "prism \"Just\"" (Just "Nothing")
        [nok' "is a value equal to 5"]
      match (Just 5) (isJustWith $ eq 5) `shouldBe`
        MatchTree True "prism \"Just\"" (Just "Just 5")
        [ok "is a value equal to 5" "5"]

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
      match (1, 2) (negationOf $ allOf [ projection "fst" fst (eq 1)
                                       , projection "snd" snd (eq 2)
                                       ])
        `shouldBe`
        MatchTree False "not all of" (Just "(1,2)")
        [ MatchTree True "projection \"fst\"" (Just "(1,2)")
          [ ok "is a value equal to 1" "1" ]
        , MatchTree True "projection \"snd\"" (Just "(1,2)")
          [ ok "is a value equal to 2" "2" ]
        ]

  describe "Projections" $ do
    context "when used with ints" $ do
      let checkTree val root = mtValue root == val && all (checkTree val) (mtSubnodes root)
      it "invariant of direction" $ property $ \x ->
        forAll (elements [Positive, Negative]) $ \dir ->
          let m = projection "id" id (eq x)
              tree = runIdentity $ m dir (Just $ Identity (x :: Int))
              treeView = prettyPrint defaultPPOptions tree
          in counterexample treeView $ checkTree (dir == Positive) tree
  
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

  describe "Tree printing" $ do
    let ?matchersOptionsAction = pure $ defaultPPOptions { ppMode = PlainText }

    it "prints trees with forward references" $ do
      let input = "a very long string that should be turn into a ref" :: String
      (input `I.shouldMatch` allOf [isEmpty, isNotEmpty]) `failureMessageIs`
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

    it "reports error when no exception thrown" $ do
      (throws (eq DivideByZero) `runMatcher` (pure 0)) `shouldReturn`
        (MatchTree False
        "action throwing ArithException that"
         Nothing
         [nok' (fromString $ "is a value equal to " ++ show DivideByZero)])

  describe "README examples" $ do
    let myDiv :: Int -> Int -> Either String Int
        myDiv _ 0 = Left "Division by zero"
        myDiv x y = Right (x `quot` y)
    it "works for positive case" $ do
      myDiv 5 0 `shouldMatch` (isLeftWith $ hasInfix "zero")

    let ?matchersOptionsAction = pure $ defaultPPOptions { ppMode = PlainText }
    it "works for negative case" $ do
      myDiv 5 0 `I.shouldMatch` (isRightWith $ eq 0)
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

    it "can fuse prisms with all of" $ do
      match (Fork (Leaf 1) (Leaf 2)) (isForkWith (isLeafWith $ eq 1) (isLeafWith $ eq 2)) `shouldBe`
        MatchTree True "prism \"Fork\"" (Just "Fork (Leaf 1) (Leaf 2)")
        [ MatchTree True "prism \"Leaf\"" (Just "Leaf 1") [ok "is a value equal to 1" "1"]
        , MatchTree True "prism \"Leaf\"" (Just "Leaf 2") [ok "is a value equal to 2" "2"]
        ]

  describe "Pretty-printing options" $ do
    it "Can deduce pretty-printing options from environment" $ do
      I.optionsFromEnv [("TERM", "xterm"), ("LANG", "en_US.UTF-8")] `shouldBe`
        defaultPPOptions

      I.optionsFromEnv [("TERM", "dumb"), ("LANG", "en_US.UTF-8")] `shouldBe`
        defaultPPOptions { ppMode = PlainText }

      I.optionsFromEnv [("TERM", "dumb"), ("LANG", "C")] `shouldBe`
        defaultPPOptions { ppMode = PlainText, ppUseUnicode = False }

      I.optionsFromEnv [] `shouldBe` defaultPPOptions { ppUseUnicode = False }
