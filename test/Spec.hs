{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec

import Data.String (IsString(..))
import GHC.IO.Exception
import Control.Exception
import Test.Matchers

ok :: Message -> Message -> MatchTree
ok msg val = Node True msg val []

nok :: Message -> Message -> MatchTree
nok msg val = Node False msg val []

type T = Either String Int

main :: IO ()
main = hspec $ do
  describe "Simple matchers" $ do

    it "can match for equality" $ do
      match 5 (eq 5) `shouldBe` ok "a value equal to 5" "5"
      match 0 (eq 1) `shouldBe` nok "a value equal to 1" "0"

    it "can match comparable types" $ do
      match 3 (gt 1) `shouldBe` ok "a value > 1" "3"
      match 3 (lt 4) `shouldBe` ok "a value < 4" "3"
      match 3 (gt 4) `shouldBe` nok "a value > 4" "3"
      match 3 (lt 1) `shouldBe` nok "a value < 1" "3"

    it "can match pairs" $ do
      match (3, 4) (tuple2 (eq 3) (gt 1)) `shouldBe`
        Node True "all of" "(3,4)"
        [ Node True "property fst is" "(3,4)"
          [ok "a value equal to 3" "3"]
        , Node True "property snd is" "(3,4)"
          [ok "a value > 1" "4"]
        ]

    it "can match Either a b" $ do
      match (Left 3 :: Either Int String) (leftIs (eq 3)) `shouldBe`
        Node True "prism Left is" "Left 3" [ok "a value equal to 3" "3"]
      match (Right "ok" :: Either Int String) (rightIs anything) `shouldBe`
        Node True "prism Right is" "Right \"ok\"" [ok "anything" "\"ok\""]

    it "can match list prefixes/suffixes/infixes" $ do
      match [1, 1, 2, 3] (startsWith [1, 1]) `shouldBe`
        ok "starts with [1,1]" "[1,1,2,3]"
      match [1, 1, 2, 3] (endsWith [2, 3]) `shouldBe`
        ok "ends with [2,3]" "[1,1,2,3]"
      match [1, 1, 2, 3] (hasInfix [1, 2]) `shouldBe`
        ok "has infix [1,2]" "[1,1,2,3]"

  describe "Test.HUnit integration" $ do

    it "can use `shouldMatch` for assertions" $ do
      ("a" :: String, 5) `shouldMatch` (tuple2 (elementsAre [eq 'a']) (gt 0))

    it "can run `shouldMatchIO` for monad assertions" $ do
      (pure 5) `shouldMatchIO` (gt 0)

  describe "Exception matching" $ do

    it "can match exceptions as normal values" $ do
      (ioError unsupportedOperation) `shouldMatchIO`
        (throws $ property "ioe_type" ioe_type (eq UnsupportedOperation))

    it "Can match any exception" $ do
      ioError unsupportedOperation `shouldMatchIO`
        throws (anything :: Matcher SomeException)

    it "does not rethrow unmatched exception" $ do
      ((throws (eq DivideByZero)) $ Just (ioError unsupportedOperation)) `shouldReturn`
        (Node False
         "exception of type ArithException matches"
         (fromString $ show unsupportedOperation)
         [nok (fromString $ "a value equal to " ++ show DivideByZero) "nothing"])
