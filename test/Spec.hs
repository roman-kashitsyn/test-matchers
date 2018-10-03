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

leafIs
  :: (Show a, Applicative f, Traversable f)
  => MatcherF f a
  -> MatcherF f (Tree a)
leafIs = prism "Leaf" (\t -> case t of Leaf x' -> Just x'; _ -> Nothing)

forkIs
  :: (Show a, Applicative f, Traversable f)
  => MatcherF f (Tree a)
  -> MatcherF f (Tree a)
  -> MatcherF f (Tree a)
forkIs leftM rightM = prism "Fork"
  (\t -> case t of Fork l' r' -> Just (l' &. r'); _ -> Nothing)
  (allOf $ matcher leftM &> matcher rightM)

treeEq
  :: (Show a, Eq a, Applicative f, Traversable f)
  => Tree a
  -> MatcherF f (Tree a)
treeEq (Leaf x) = leafIs (eq x)
treeEq (Fork l r) = forkIs (treeEq l) (treeEq r)

ok :: Message -> Message -> Message -> MatchTree
ok msg msg_ val = MatchTree True msg msg_ val []

nok :: Message -> Message -> Message -> MatchTree
nok msg msg_ val = MatchTree False msg msg_ val []

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
      match 5 (eq 5) `shouldBe` ok "a value equal to 5" "a value not equal to 5" "5"
      match 0 (eq 1) `shouldBe` nok "a value equal to 1" "a value not equal to 1" "0"

    it "can match comparable types" $ do
      match 3 (gt 1) `shouldBe` ok "a value > 1" "a value ≤ 1" "3"
      match 1 (ge 1) `shouldBe` ok "a value ≥ 1" "a value < 1" "1"
      match 3 (ge 1) `shouldBe` ok "a value ≥ 1" "a value < 1" "3"

      match 3 (lt 4) `shouldBe` ok "a value < 4" "a value ≥ 4" "3"
      match 3 (le 4) `shouldBe` ok "a value ≤ 4" "a value > 4" "3"
      match 3 (le 3) `shouldBe` ok "a value ≤ 3" "a value > 3" "3"

      match 3 (gt 4) `shouldBe` nok "a value > 4" "a value ≤ 4" "3"
      match 3 (ge 4) `shouldBe` nok "a value ≥ 4" "a value < 4" "3"

      match 3 (lt 1) `shouldBe` nok "a value < 1" "a value ≥ 1" "3"
      match 3 (le 1) `shouldBe` nok "a value ≤ 1" "a value > 1" "3"

    it "can match pairs" $ do
      match (3, 4) (tuple2 (eq 3) (gt 1)) `shouldBe`
        MatchTree True "all of" "not all of" "(3,4)"
        [ MatchTree True "property fst is" "property fst is not" "(3,4)"
          [ok "a value equal to 3" "a value not equal to 3" "3"]
        , MatchTree True "property snd is" "property snd is not" "(3,4)"
          [ok "a value > 1" "a value ≤ 1" "4"]
        ]

    it "can match Either a b" $ do
      match (Left 3 :: Either Int String) (leftIs (eq 3)) `shouldBe`
        MatchTree True
        "prism Left is"
        "prism Left is not"
        "Left 3" [ok "a value equal to 3" "a value not equal to 3" "3"]
      match (Right "ok" :: Either Int String) (rightIs anything) `shouldBe`
        MatchTree True
        "prism Right is"
        "prism Right is not"
        "Right \"ok\"" [ok "anything" "nothing" "\"ok\""]

    it "can match list prefixes/suffixes/infixes" $ do
      match [1, 1, 2, 3] (startsWith [1, 1]) `shouldBe`
        ok "starts with [1,1]" "does not start with [1,1]" "[1,1,2,3]"
      match [1, 1, 2, 3] (endsWith [2, 3]) `shouldBe`
        ok "ends with [2,3]" "does not end with [2,3]" "[1,1,2,3]"
      match [1, 1, 2, 3] (hasInfix [1, 2]) `shouldBe`
        ok "has infix [1,2]" "does not have infix [1,2]" "[1,1,2,3]"

    it "can match lists" $ do
      match ([] :: [Int])  (elementsAre []) `shouldBe`
        MatchTree True "has 0 elements" "does not have 0 elements" "[]"
        [ok "number of elements is 0" "number of elements is not 0" "0"]

      match [] (elementsAre [eq 5]) `shouldBe`
        MatchTree False "has 1 element" "does not have 1 element" "[]"
        [ nok
          "a value equal to 5"
          "a value not equal to 5"
          "nothing"
        , nok
          "number of elements is 1"
          "number of elements is not 1"
          "0"
        ]
      match [5, 7] (elementsAre [eq 5]) `shouldBe`
        MatchTree False "has 1 element" "does not have 1 element" "[5,7]"
        [ ok "a value equal to 5" "a value not equal to 5" "5"
        , nok "number of elements is 1" "number of elements is not 1" "2"
        ]
      match [5, 7] (elementsAre [eq 5, gt 5]) `shouldBe`
        MatchTree True "has 2 elements" "does not have 2 elements" "[5,7]"
        [ ok "a value equal to 5" "a value not equal to 5" "5"
        , ok "a value > 5" "a value ≤ 5" "7"
        , ok "number of elements is 2" "number of elements is not 2" "2"
        ]

  describe "Container matching" $ do
    it "can check if container is empty" $ do
      match (Nothing :: Maybe Int) isEmpty `shouldBe` ok "is empty" "is not empty" "Nothing"
      match ([] :: [Int]) isEmpty `shouldBe` ok "is empty" "is not empty" "[]"
      match (Just 1) isEmpty `shouldBe` nok "is empty" "is not empty" "Just 1"
      match [1,2] isEmpty `shouldBe` nok "is empty" "is not empty" "[1,2]"

    it "can check the length of a container" $ do
      match (Just 1) (lengthIs $ gt 0) `shouldBe`
        MatchTree True "property length is" "property length is not" "Just 1"
        [ ok "a value > 0" "a value ≤ 0" "1" ]
      match [1,2,3] (lengthIs $ eq 4) `shouldBe`
        MatchTree False "property length is" "property length is not" "[1,2,3]"
        [ nok "a value equal to 4" "a value not equal to 4" "3" ]

  describe "Test.HUnit integration" $ do

    it "can use `shouldMatch` for assertions" $ do
      ("a" :: String, 5) `shouldMatch` (tuple2 (elementsAre [eq 'a']) (gt 0))

    it "can run `shouldMatchIO` for monad assertions" $ do
      (pure 5) `shouldMatchIO` (gt 0)

  describe "Exception matching" $ do

    it "can match exceptions as normal values" $ do
      (ioError unsupportedOperation) `shouldMatchIO`
        (throws $ property "ioe_type" ioe_type (eq UnsupportedOperation))

    it "can match any exception" $ do
      ioError unsupportedOperation `shouldMatchIO`
        throws (anything :: Matcher SomeException)

    it "does not rethrow unmatched exception" $ do
      ((throws (eq DivideByZero)) $ Just (ioError unsupportedOperation)) `shouldReturn`
        (MatchTree False
         "action throwing ArithException that is"
         "action not throwing ArithException that is"
         (fromString $ show unsupportedOperation)
         [nok
          (fromString $ "a value equal to " ++ show DivideByZero)
          (fromString $ "a value not equal to " ++ show DivideByZero)
          "nothing"])

  describe "README examples" $ withNoColor $ do
    let div :: Int -> Int -> Either String Int
        div _ 0 = Left "Division by zero"
        div x y = Right (x `quot` y)
    it "works for positive case" $ do
      div 5 0 `shouldMatch` (leftIs $ hasInfix "zero")
    it "works for negative case" $ do
      div 5 0 `shouldMatch` (rightIs $ eq 0)
        `failureMessageIs`
        (intercalate "\n" [ "✘ prism Right is ← Left \"Division by zero\""
                          , "  ✘ a value equal to 0 ← nothing"
                          ])

  describe "Custom matchers" $ do
    it "can match trees" $ do
      let t = (Fork (Fork (Leaf 5) (Leaf 7)) (Leaf 10))
      t `shouldMatch` treeEq t
