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
import Data.Functor.Identity (Identity(..), runIdentity)
import qualified Data.Text as T
import Test.Matchers
import Test.Matchers.LexShow (LexResult(Full, Partial), lexShow, tokenText)
import Test.QuickCheck
  ( Arbitrary(arbitrary, shrink)
  , Property
  , counterexample
  , discard
  , elements
  , forAll
  , oneof
  , sized
  , (===)
  )
import Test.Tasty (TestTree, defaultMain, testGroup, adjustOption)
import Test.Tasty.QuickCheck (testProperty, QuickCheckMaxSize(..))

data PTree
  = PEmpty
  | PInt Int
  | PDouble Double
  | PString String
  -- We use a fancy symbol here to check that operators are lexed
  -- correctly.
  | PTree :|: PTree
  deriving (Show, Eq)

instance Arbitrary PTree where
  arbitrary = sized arb'
    where arb' n | n <= 0  = pure PEmpty
          arb' 1 = oneof [ PInt <$> arbitrary
                         , PDouble <$> arbitrary
                         , PString <$> arbitrary
                         ]
          arb' k = (:|:) <$> (arb' $ k `quot` 2) <*> (arb' $ k `quot` 2)
  shrink (l :|: r) = [PEmpty, l, r] ++ [l' :|: r' | (l', r') <- shrink (l, r)]
  shrink _ = []

prop_proj_dir_invariant :: Int -> Property
prop_proj_dir_invariant =
  let checkTree val root = mtValue root == val && all (checkTree val) (mtSubnodes root)
  in \x -> forAll (elements [Positive, Negative]) $ \dir ->
    let m = projection "id" id (eq x)
        tree = runIdentity $ m dir (Just $ Identity (x :: Int))
        treeView = prettyPrint defaultPPOptions tree
    in counterexample treeView $ checkTree (dir == Positive) tree

prop_lex_keeps_formatting :: PTree -> Property
prop_lex_keeps_formatting t = let showed = show t
                              in case lexShow maxBound showed of
                                   Full tokens -> T.concat (map tokenText tokens) === (T.pack showed)
                                   Partial _ -> discard

propTests :: TestTree
propTests
  = testGroup "Properties"
    [ testGroup "Projections"
      [ testProperty "invariant of direction" prop_proj_dir_invariant
      ]
    , adjustOption (`min` QuickCheckMaxSize 16) $
      testGroup "Pretty Printing"
      [ testProperty "Lexing preserves formatting" prop_lex_keeps_formatting
      ]
    ]

main :: IO ()
main = defaultMain propTests
