{-
Copyright 2018-2019 Google LLC

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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Control.Exception
import Data.List (intercalate)
import Data.String (IsString(..))
import GHC.IO.Exception
import Test.Hspec
import Test.HUnit.Lang (FailureReason(Reason), HUnitFailure(..))
import Test.Matchers
import Test.Matchers.HUnit
import qualified Test.Matchers.HUnit.Implicit as I
import Test.Matchers.Message

default (Integer, Double)

data Tree a = Leaf a | Fork (Tree a) (Tree a) deriving (Show, Eq)

simplifyMessage :: Message -> Message
simplifyMessage msg = case msg of
  HCat ms -> mconcat $ map simplifyMessage ms
  FancyChar c _ -> Str [c]
  Value s -> Str s
  Symbol s -> Str s
  Space -> Str " "
  Empty -> Empty
  s@(Str _) -> s

simplifyTree :: MatchTree -> MatchTree
simplifyTree t = t { mtDescription = simplifyMessage (mtDescription t)
                   , mtSubnodes = map simplifyTree (mtSubnodes t)
                   }

isLeafWith
  :: (Show a, Applicative f, Traversable f)
  => MatcherF f a
  -> MatcherF f (Tree a)
isLeafWith m = prism "Leaf" (\case Leaf x' -> Just x'; _ -> Nothing) [m]

isForkWith
  :: (Show a, Applicative f, Traversable f)
  => MatcherF f (Tree a)
  -> MatcherF f (Tree a)
  -> MatcherF f (Tree a)
isForkWith leftM rightM = prism "Fork" p
                          ([labeled "left" leftM] &> [labeled "right" rightM])
  where p (Fork l r) = Just (l &. r)
        p _ = Nothing

treeEq
  :: (Show a, Eq a, Applicative f, Traversable f)
  => Tree a
  -> MatcherF f (Tree a)
treeEq (Leaf x) = isLeafWith (eq x)
treeEq (Fork l r) = isForkWith (treeEq l) (treeEq r)

ok :: Message -> String -> MatchTree
ok msg val = MatchTree True msg [] (Just val) []

nok :: Message -> String -> MatchTree
nok msg val = MatchTree False msg [] (Just val) []

nok' :: Message -> MatchTree
nok' msg = MatchTree False msg [] Nothing []

mtOk, mtNok :: Message -> Maybe String -> [MatchTree] -> MatchTree
mtOk msg = MatchTree True msg []
mtNok msg = MatchTree False msg []

mtOkL, mtNokL :: Message -> String -> Maybe String -> [MatchTree] -> MatchTree
mtOkL msg label = MatchTree True msg [label]
mtNokL msg label = MatchTree False msg [label]

shouldBeEquiv :: HasCallStack => MatchTree -> MatchTree -> Expectation
shouldBeEquiv lhs rhs = simplifyTree lhs `shouldBe` simplifyTree rhs

failureMessageIs :: Expectation -> String -> Expectation
failureMessageIs test msg =
  test `shouldThrow` (\(HUnitFailure _ reason) -> reason == Reason msg)

main :: IO ()
main = hspec $ do
  describe "Simple matchers" $ do
    it "can match for equality" $ do
      match (eq 5) 5 `shouldBeEquiv` ok "is a value equal to 5" "5"
      match (eq 1) 0 `shouldBeEquiv` nok "is a value equal to 1" "0"
      match (ne 1) 0 `shouldBeEquiv` ok "is a value not equal to 1" "0"

    it "can match comparable types" $ do
      match (gt 1) 3 `shouldBeEquiv` ok "is a value > 1" "3"
      match (ge 1) 1 `shouldBeEquiv` ok "is a value ≥ 1" "1"
      match (ge 1) 3 `shouldBeEquiv` ok "is a value ≥ 1" "3"

      match (lt 4) 3 `shouldBeEquiv` ok "is a value < 4" "3"
      match (le 4) 3 `shouldBeEquiv` ok "is a value ≤ 4" "3"
      match (le 3) 3 `shouldBeEquiv` ok "is a value ≤ 3" "3"

      match (gt 4) 3 `shouldBeEquiv` nok "is a value > 4" "3"
      match (ge 4) 3 `shouldBeEquiv` nok "is a value ≥ 4" "3"

      match (lt 1) 3 `shouldBeEquiv` nok "is a value < 1" "3"
      match (le 1) 3 `shouldBeEquiv` nok "is a value ≤ 1" "3"

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
      match (tuple2 (eq 3) (gt 1)) (3, 4) `shouldBeEquiv`
        mtOk "all of" (Just "(3,4)")
        [ mtOkL "is a value equal to 3" "fst" (Just "3") []
        , mtOkL "is a value > 1" "snd" (Just "4") []
        ]

      match (tuple3 (eq 3) (gt 1) (gt 3)) (3, 4, 5) `shouldBeEquiv`
        mtOk "all of" (Just "(3,4,5)")
        [ mtOkL "is a value equal to 3" "_1" (Just "3") []
        , mtOkL "is a value > 1" "_2" (Just "4") []
        , mtOkL "is a value > 3" "_3" (Just "5") []
        ]
      match (tuple3 (lt 4) (lt 4) (lt 4)) (3, 4, 5) `shouldBeEquiv`
        mtNok "all of" (Just "(3,4,5)")
        [ mtOkL "is a value < 4" "_1" (Just "3") []
        , mtNokL "is a value < 4" "_2" (Just "4") []
        , mtNokL "is a value < 4" "_3" (Just "5") []
        ]

    it "can match Maybe a" $ do
      match isNothing (Nothing :: Maybe Int) `shouldBeEquiv`
        ok "is Nothing" "Nothing"
      match isNothing (Just 5) `shouldBeEquiv`
        nok "is Nothing" "Just 5"
      match (negationOf isNothing) (Nothing :: Maybe Int) `shouldBeEquiv`
        nok "is not Nothing" "Nothing"
      match (negationOf isNothing) (Just 5) `shouldBeEquiv`
        ok "is not Nothing" "Just 5"

      match (isJustWith $ eq 5) (Nothing :: Maybe Int) `shouldBeEquiv`
        mtNok "prism \"Just\"" (Just "Nothing")
        [nok' "is a value equal to 5"]
      match (isJustWith $ eq 5) (Just 5) `shouldBeEquiv`
        mtOk "prism \"Just\"" (Just "Just 5")
        [ok "is a value equal to 5" "5"]

    it "can match Either a b" $ do
      match (isLeftWith (eq 3)) (Left 3 :: Either Int String) `shouldBeEquiv`
        mtOk "prism \"Left\"" (Just "Left 3") [ok "is a value equal to 3" "3"]
      match (isRightWith anything) (Right "ok" :: Either Int String) `shouldBeEquiv`
        mtOk "prism \"Right\"" (Just "Right \"ok\"") [ok "anything" "\"ok\""]

    it "can match list prefixes/suffixes/infixes" $ do
      match (startsWith [1, 1]) [1, 1, 2, 3] `shouldBeEquiv`
        ok "starts with [1,1]" "[1,1,2,3]"
      match (endsWith [2, 3]) [1, 1, 2, 3] `shouldBeEquiv`
        ok "ends with [2,3]" "[1,1,2,3]"
      match (hasInfix [1, 2]) [1, 1, 2, 3] `shouldBeEquiv`
        ok "has infix [1,2]" "[1,1,2,3]"

    it "can match lists" $ do
      match (elementsAre []) ([] :: [Int])  `shouldBeEquiv`
        mtOk "container such that" (Just "[]")
        [ok "number of elements is 0" "0"]

      match (elementsAre [eq 5]) [] `shouldBeEquiv`
        mtNok "container such that" (Just "[]")
        [ nok' "is a value equal to 5"
        , nok "number of elements is 1" "0"
        ]
      match (elementsAre [eq 5]) [5, 7] `shouldBeEquiv`
        mtNok "container such that" (Just "[5,7]")
        [ ok "is a value equal to 5" "5"
        , nok "number of elements is 1" "2"
        ]
      match (elementsAre [eq 5, gt 5]) [5, 7] `shouldBeEquiv`
        mtOk "container such that" (Just "[5,7]")
        [ ok "is a value equal to 5" "5"
        , ok "is a value > 5" "7"
        , ok "number of elements is 2" "2"
        ]

    it "can match each element of a list" $ do
      match  (each $ gt 0) [1,2] `shouldBeEquiv`
        mtOk "each element of the container" (Just "[1,2]")
        [ ok "is a value > 0" "1"
        , ok "is a value > 0" "2"
        ]

      match (each $ gt 0) [-1,1] `shouldBeEquiv`
        mtNok "each element of the container" (Just "[-1,1]")
        [ nok "is a value > 0" "-1"
        , ok "is a value > 0" "1"
        ]

      match (each $ gt 0) [] `shouldBeEquiv`
        mtOk "each element of the container" (Just "[]")
        [ nok' "is a value > 0" ]

      match (negationOf $ each $ gt 0) [] `shouldBeEquiv`
        mtNok "container has at least one element that" (Just "[]")
        [ nok' "is a value > 0" ]

      match (negationOf $ each $ lt 5) [0,5] `shouldBeEquiv`
        mtOk "container has at least one element that" (Just "[0,5]")
        [ ok "is a value < 5" "0"
        , nok "is a value < 5" "5"
        ]

    it "can aggregate matchers" $ do
      match (allOf [gt 0, lt 5]) 1 `shouldBeEquiv`
        mtOk "all of" (Just "1")
        [ ok "is a value > 0" "1"
        , ok "is a value < 5" "1"
        ]
      match (allOf [gt 1, lt 5]) 1 `shouldBeEquiv`
        mtNok "all of" (Just "1")
        [ nok "is a value > 1" "1"
        , ok "is a value < 5" "1"
        ]
      match (oneOf [lt 1, ne 0]) 1 `shouldBeEquiv`
        mtOk "one of" (Just "1")
        [ nok "is a value < 1" "1"
        , ok "is a value not equal to 0" "1"
        ]
      match (oneOf [lt (-1), gt 1]) 1 `shouldBeEquiv`
        mtNok "one of" (Just "1")
        [ nok "is a value < -1" "1"
        , nok "is a value > 1" "1"
        ]
      match (negationOf $ allOf [ projection "fst" fst [eq 1]
                                , projection "snd" snd [eq 2]
                                ]) (1, 2)
        `shouldBeEquiv`
        mtNok "not all of" (Just "(1,2)")
        [ mtOk "projection \"fst\"" (Just "(1,2)")
          [ ok "is a value equal to 1" "1" ]
        , mtOk "projection \"snd\"" (Just "(1,2)")
          [ ok "is a value equal to 2" "2" ]
        ]

  describe "Container matching" $ do
    it "can check if container is empty" $ do
      match isEmpty (Nothing :: Maybe Int) `shouldBeEquiv` ok "is empty" "Nothing"
      match isEmpty ([] :: [Int]) `shouldBeEquiv` ok "is empty" "[]"
      match isEmpty (Just 1) `shouldBeEquiv` nok "is empty" "Just 1"
      match isEmpty [1,2] `shouldBeEquiv` nok "is empty" "[1,2]"
      match isNotEmpty [1] `shouldBeEquiv` ok "is not empty" "[1]"
      match isNotEmpty ([] :: [Int]) `shouldBeEquiv` nok "is not empty" "[]"

    it "can check the length of a container" $ do
      match (hasLength $ gt 0) (Just 1) `shouldBeEquiv`
        mtOk "projection \"length\"" (Just "Just 1")
        [ ok "is a value > 0" "1" ]
      match (hasLength $ eq 4) [1,2,3] `shouldBeEquiv`
        mtNok "projection \"length\"" (Just "[1,2,3]")
        [ nok "is a value equal to 4" "3" ]

  describe "Test.HUnit integration" $ do

    it "can use `shouldMatch` for assertions" $
      ("a" :: String, 5) `shouldMatch` tuple2 (elementsAre [eq 'a']) (gt 0)

    it "can run `shouldMatchIO` for monad assertions" $
      pure 5 `shouldMatchIO` gt 0

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

    it "Respects the useUnicode setting" $ do
      let ?matchersOptionsAction = pure $ defaultPPOptions { ppMode = PlainText
                                                           , ppUseUnicode = False
                                                           }
      ("test" :: String) `I.shouldMatch` allOf [isEmpty, isNotEmpty]
      `failureMessageIs`
        intercalate "\n"
        [ "[x] all of <- \"test\""
        , "  [x] is empty <- \"test\""
        , "  [v] is not empty <- \"test\""
        ]

    it "can display simple labels" $
      Fork (Leaf 1) (Leaf 2) `I.shouldMatch` isForkWith (isLeafWith $ eq 1) (isLeafWith $ eq 1)
      `failureMessageIs`
        intercalate "\n"
        [ "✘ prism \"Fork\" ← <1>"
        , "  ✔ [left] prism \"Leaf\" ← Leaf 1"
        , "    ✔ is a value equal to 1 ← 1"
        , "  ✘ [right] prism \"Leaf\" ← Leaf 2"
        , "    ✘ is a value equal to 1 ← 2"
        , "where:"
        , "  <1> Fork (Leaf 1) (Leaf 2)"
        ]

    it "can display composed labels" $
      2 `I.shouldMatch` labeled "foo"  (labeled "bar" $ eq 0)
      `failureMessageIs`
        "✘ [foo.bar] is a value equal to 0 ← 2"

  describe "Sub-value abbreviation" $ do
    let ?matchersOptionsAction = pure $ defaultPPOptions { ppMode = PlainText }

    it "can abbreviate sub-values in messages" $ do
      let input = "a very long string that should be turn into a ref" :: String
      (input, 1 :: Int) `I.shouldMatch` projection "first" fst [isEmpty, isNotEmpty]
       `failureMessageIs`
        intercalate "\n"
        [ "✘ projection \"first\" ← <1>"
        , "  ✘ is empty ← <2>"
        , "  ✔ is not empty ← <2>"
        , "where:"
        , "  <1> (<2>,1)"
        , "  <2> " ++ show input
        ]


  describe "Exception matching" $ do

    it "can match exceptions as normal values" $
      ioError unsupportedOperation `shouldMatchIO`
        throws (projection "ioe_type" ioe_type [eq UnsupportedOperation])

    it "can match any exception" $
      ioError unsupportedOperation `shouldMatchIO`
        throws (anything :: Matcher SomeException)

    it "does not rethrow unmatched exception" $
      fmap simplifyTree (throws (eq DivideByZero) `runMatcher` ioError unsupportedOperation)
        `shouldReturn`
        mtNok
        "action throwing ArithException that"
        (Just $ show unsupportedOperation)
        [nok' (fromString $ "is a value equal to " ++ show DivideByZero)]

    it "reports error when no exception thrown" $
      fmap simplifyTree (throws (eq DivideByZero) `runMatcher` pure 0) `shouldReturn`
        mtNok
        "action throwing ArithException that"
        Nothing
        [nok' (fromString $ "is a value equal to " ++ show DivideByZero)]

  describe "README examples" $ do
    let myDiv :: Int -> Int -> Either String Int
        myDiv _ 0 = Left "Division by zero"
        myDiv x y = Right (x `quot` y)

    it "works for positive case" $
      myDiv 5 0 `shouldMatch` isLeftWith (hasInfix "zero")

    let ?matchersOptionsAction = pure $ defaultPPOptions { ppMode = PlainText }

    it "works for negative case" $
      myDiv 5 0 `I.shouldMatch` isRightWith (eq 0)
        `failureMessageIs`
        intercalate "\n" [ "✘ prism \"Right\" ← <1>"
                         , "  ✘ is a value equal to 0"
                         , "where:"
                         , "  <1> Left \"Division by zero\""
                         ]

  describe "Custom matchers" $ do
    it "can match trees" $ do
      let t = Fork (Fork (Leaf 5) (Leaf 7)) (Leaf 10)
      t `shouldMatch` treeEq t

    it "can fuse prisms with all of" $
      match (isForkWith (isLeafWith $ eq 1) (isLeafWith $ eq 2)) (Fork (Leaf 1) (Leaf 2)) `shouldBeEquiv`
        mtOk "prism \"Fork\"" (Just "Fork (Leaf 1) (Leaf 2)")
        [ mtOkL "prism \"Leaf\"" "left" (Just "Leaf 1")
          [ok "is a value equal to 1" "1"]
        , mtOkL "prism \"Leaf\"" "right" (Just "Leaf 2")
          [ok "is a value equal to 2" "2"]
        ]

  describe "Pretty-printing options" $
    it "Can deduce pretty-printing options from environment" $ do
      I.optionsFromEnv [("TERM", "xterm"), ("LANG", "en_US.UTF-8")] `shouldBe`
        defaultPPOptions

      I.optionsFromEnv [("TERM", "dumb"), ("LANG", "en_US.UTF-8")] `shouldBe`
        defaultPPOptions { ppMode = PlainText }

      I.optionsFromEnv [("TERM", "dumb"), ("LANG", "C")] `shouldBe`
        defaultPPOptions { ppMode = PlainText, ppUseUnicode = False }

      I.optionsFromEnv [] `shouldBe` defaultPPOptions { ppUseUnicode = False }
