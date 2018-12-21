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
import GHC.IO.Exception
import Control.Exception
import Test.Matchers
import Test.Matchers.Message
import Test.Matchers.HUnit
import qualified Test.Matchers.HUnit.Implicit as I
import Test.HUnit.Lang (HUnitFailure(..), FailureReason(Reason))

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
isLeafWith = prism "Leaf" (\t -> case t of Leaf x' -> Just x'; _ -> Nothing)

isForkWith
  :: (Show a, Applicative f, Traversable f)
  => MatcherF f (Tree a)
  -> MatcherF f (Tree a)
  -> MatcherF f (Tree a)
isForkWith leftM rightM = prismWithSet "Fork" p
                          ( matcher (labeled "left" leftM) &>
                            matcher (labeled "right" rightM)
                          )
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
mtOk msg mval sub = MatchTree True msg [] mval sub
mtNok msg mval sub = MatchTree False msg [] mval sub

shouldBeEquiv :: HasCallStack => MatchTree -> MatchTree -> Expectation
shouldBeEquiv lhs rhs = (simplifyTree lhs) `shouldBe` (simplifyTree rhs)

failureMessageIs :: Expectation -> String -> Expectation
failureMessageIs test msg =
  test `shouldThrow` (\(HUnitFailure _ reason) -> reason == Reason msg)

main :: IO ()
main = hspec $ do
  describe "Simple matchers" $ do
    it "can match for equality" $ do
      match 5 (eq 5) `shouldBeEquiv` ok "is a value equal to 5" "5"
      match 0 (eq 1) `shouldBeEquiv` nok "is a value equal to 1" "0"
      match 0 (ne 1) `shouldBeEquiv` ok "is a value not equal to 1" "0"

    it "can match comparable types" $ do
      match 3 (gt 1) `shouldBeEquiv` ok "is a value > 1" "3"
      match 1 (ge 1) `shouldBeEquiv` ok "is a value ≥ 1" "1"
      match 3 (ge 1) `shouldBeEquiv` ok "is a value ≥ 1" "3"

      match 3 (lt 4) `shouldBeEquiv` ok "is a value < 4" "3"
      match 3 (le 4) `shouldBeEquiv` ok "is a value ≤ 4" "3"
      match 3 (le 3) `shouldBeEquiv` ok "is a value ≤ 3" "3"

      match 3 (gt 4) `shouldBeEquiv` nok "is a value > 4" "3"
      match 3 (ge 4) `shouldBeEquiv` nok "is a value ≥ 4" "3"

      match 3 (lt 1) `shouldBeEquiv` nok "is a value < 1" "3"
      match 3 (le 1) `shouldBeEquiv` nok "is a value ≤ 1" "3"

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
      match (3, 4) (tuple2 (eq 3) (gt 1)) `shouldBeEquiv`
        mtOk "all of" (Just "(3,4)")
        [ mtOk "projection \"fst\"" (Just "(3,4)")
          [ok "is a value equal to 3" "3"]
        , mtOk "projection \"snd\"" (Just "(3,4)")
          [ok "is a value > 1" "4"]
        ]

      match (3, 4, 5) (tuple3 (eq 3) (gt 1) (gt 3)) `shouldBeEquiv`
        mtOk "all of" (Just "(3,4,5)")
        [ mtOk "projection \"#1\"" (Just "(3,4,5)")
          [ok "is a value equal to 3" "3"]
        , mtOk "projection \"#2\"" (Just "(3,4,5)")
          [ok "is a value > 1" "4"]
        , mtOk "projection \"#3\"" (Just "(3,4,5)")
          [ok "is a value > 3" "5"]
        ]
      match (3, 4, 5) (tuple3 (lt 4) (lt 4) (lt 4)) `shouldBeEquiv`
        mtNok "all of" (Just "(3,4,5)")
        [ mtOk "projection \"#1\"" (Just "(3,4,5)")
          [ok "is a value < 4" "3"]
        , mtNok "projection \"#2\"" (Just "(3,4,5)")
          [nok "is a value < 4" "4"]
        , mtNok "projection \"#3\"" (Just "(3,4,5)")
          [nok "is a value < 4" "5"]
        ]

    it "can match Maybe a" $ do
      match (Nothing :: Maybe Int) isNothing `shouldBeEquiv`
        ok "is Nothing" "Nothing"
      match (Just 5) isNothing `shouldBeEquiv`
        nok "is Nothing" "Just 5"
      match (Nothing :: Maybe Int) (negationOf isNothing) `shouldBeEquiv`
        nok "is not Nothing" "Nothing"
      match (Just 5) (negationOf isNothing) `shouldBeEquiv`
        ok "is not Nothing" "Just 5"

      match (Nothing :: Maybe Int) (isJustWith $ eq 5) `shouldBeEquiv`
        mtNok "prism \"Just\"" (Just "Nothing")
        [nok' "is a value equal to 5"]
      match (Just 5) (isJustWith $ eq 5) `shouldBeEquiv`
        mtOk "prism \"Just\"" (Just "Just 5")
        [ok "is a value equal to 5" "5"]

    it "can match Either a b" $ do
      match (Left 3 :: Either Int String) (isLeftWith (eq 3)) `shouldBeEquiv`
        mtOk "prism \"Left\"" (Just "Left 3") [ok "is a value equal to 3" "3"]
      match (Right "ok" :: Either Int String) (isRightWith anything) `shouldBeEquiv`
        mtOk "prism \"Right\"" (Just "Right \"ok\"") [ok "anything" "\"ok\""]

    it "can match list prefixes/suffixes/infixes" $ do
      match [1, 1, 2, 3] (startsWith [1, 1]) `shouldBeEquiv`
        ok "starts with [1,1]" "[1,1,2,3]"
      match [1, 1, 2, 3] (endsWith [2, 3]) `shouldBeEquiv`
        ok "ends with [2,3]" "[1,1,2,3]"
      match [1, 1, 2, 3] (hasInfix [1, 2]) `shouldBeEquiv`
        ok "has infix [1,2]" "[1,1,2,3]"

    it "can match lists" $ do
      match ([] :: [Int])  (elementsAre []) `shouldBeEquiv`
        mtOk "container such that" (Just "[]")
        [ok "number of elements is 0" "0"]

      match [] (elementsAre [eq 5]) `shouldBeEquiv`
        mtNok "container such that" (Just "[]")
        [ nok' "is a value equal to 5"
        , nok "number of elements is 1" "0"
        ]
      match [5, 7] (elementsAre [eq 5]) `shouldBeEquiv`
        mtNok "container such that" (Just "[5,7]")
        [ ok "is a value equal to 5" "5"
        , nok "number of elements is 1" "2"
        ]
      match [5, 7] (elementsAre [eq 5, gt 5]) `shouldBeEquiv`
        mtOk "container such that" (Just "[5,7]")
        [ ok "is a value equal to 5" "5"
        , ok "is a value > 5" "7"
        , ok "number of elements is 2" "2"
        ]

    it "can match each element of a list" $ do
      match [1,2] (each $ gt 0) `shouldBeEquiv`
        mtOk "each element of the container" (Just "[1,2]")
        [ ok "is a value > 0" "1"
        , ok "is a value > 0" "2"
        ]

      match [-1,1] (each $ gt 0) `shouldBeEquiv`
        mtNok "each element of the container" (Just "[-1,1]")
        [ nok "is a value > 0" "-1"
        , ok "is a value > 0" "1"
        ]

      match [] (each $ gt 0) `shouldBeEquiv`
        mtOk "each element of the container" (Just "[]")
        [ nok' "is a value > 0" ]

      match [] (negationOf $ each $ gt 0) `shouldBeEquiv`
        mtNok "container has at least one element that" (Just "[]")
        [ nok' "is a value > 0" ]

      match [0,5] (negationOf $ each $ lt 5) `shouldBeEquiv`
        mtOk "container has at least one element that" (Just "[0,5]")
        [ ok "is a value < 5" "0"
        , nok "is a value < 5" "5"
        ]

    it "can aggregate matchers" $ do
      match 1 (allOf [gt 0, lt 5]) `shouldBeEquiv`
        mtOk "all of" (Just "1")
        [ ok "is a value > 0" "1"
        , ok "is a value < 5" "1"
        ]
      match 1 (allOf [gt 1, lt 5]) `shouldBeEquiv`
        mtNok "all of" (Just "1")
        [ nok "is a value > 1" "1"
        , ok "is a value < 5" "1"
        ]
      match 1 (oneOf [lt 1, ne 0]) `shouldBeEquiv`
        mtOk "one of" (Just "1")
        [ nok "is a value < 1" "1"
        , ok "is a value not equal to 0" "1"
        ]
      match 1 (oneOf [lt (-1), gt 1]) `shouldBeEquiv`
        mtNok "one of" (Just "1")
        [ nok "is a value < -1" "1"
        , nok "is a value > 1" "1"
        ]
      match (1, 2) (negationOf $ allOf [ projection "fst" fst (eq 1)
                                       , projection "snd" snd (eq 2)
                                       ])
        `shouldBeEquiv`
        mtNok "not all of" (Just "(1,2)")
        [ mtOk "projection \"fst\"" (Just "(1,2)")
          [ ok "is a value equal to 1" "1" ]
        , mtOk "projection \"snd\"" (Just "(1,2)")
          [ ok "is a value equal to 2" "2" ]
        ]
  
  describe "Container matching" $ do
    it "can check if container is empty" $ do
      match (Nothing :: Maybe Int) isEmpty `shouldBeEquiv` ok "is empty" "Nothing"
      match ([] :: [Int]) isEmpty `shouldBeEquiv` ok "is empty" "[]"
      match (Just 1) isEmpty `shouldBeEquiv` nok "is empty" "Just 1"
      match [1,2] isEmpty `shouldBeEquiv` nok "is empty" "[1,2]"
      match [1] isNotEmpty `shouldBeEquiv` ok "is not empty" "[1]"
      match ([] :: [Int]) isNotEmpty `shouldBeEquiv` nok "is not empty" "[]"

    it "can check the length of a container" $ do
      match (Just 1) (hasLength $ gt 0) `shouldBeEquiv`
        mtOk "projection \"length\"" (Just "Just 1")
        [ ok "is a value > 0" "1" ]
      match [1,2,3] (hasLength $ eq 4) `shouldBeEquiv`
        mtNok "projection \"length\"" (Just "[1,2,3]")
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

    it "Respects the useUnicode setting" $ do
      let ?matchersOptionsAction = pure $ defaultPPOptions { ppMode = PlainText
                                                           , ppUseUnicode = False
                                                           }
      (("test" :: String) `I.shouldMatch` allOf [isEmpty, isNotEmpty]) `failureMessageIs`
        intercalate "\n"
        [ "[x] all of <- \"test\""
        , "  [x] is empty <- \"test\""
        , "  [v] is not empty <- \"test\""
        ]
  
  describe "Exception matching" $ do

    it "can match exceptions as normal values" $ do
      (ioError unsupportedOperation) `shouldMatchIO`
        (throws $ projection "ioe_type" ioe_type (eq UnsupportedOperation))

    it "can match any exception" $ do
      ioError unsupportedOperation `shouldMatchIO`
        throws (anything :: Matcher SomeException)

    it "does not rethrow unmatched exception" $ do
      fmap simplifyTree (throws (eq DivideByZero) `runMatcher` ioError unsupportedOperation)
        `shouldReturn`
        (mtNok
         "action throwing ArithException that"
         (Just $ show unsupportedOperation)
         [nok' (fromString $ "is a value equal to " ++ show DivideByZero)
         ])

    it "reports error when no exception thrown" $ do
      fmap simplifyTree (throws (eq DivideByZero) `runMatcher` (pure 0)) `shouldReturn`
        (mtNok
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
      match (Fork (Leaf 1) (Leaf 2)) (isForkWith (isLeafWith $ eq 1) (isLeafWith $ eq 2)) `shouldBeEquiv`
        mtOk "prism \"Fork\"" (Just "Fork (Leaf 1) (Leaf 2)")
        [ MatchTree True "prism \"Leaf\"" ["left"] (Just "Leaf 1") [ok "is a value equal to 1" "1"]
        , MatchTree True "prism \"Leaf\"" ["right"] (Just "Leaf 2") [ok "is a value equal to 2" "2"]
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
