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
import Data.Functor.Identity (Identity (..), runIdentity)
import Test.Matchers
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
  ( counterexample
  , elements
  , forAll
  )

propTests :: TestTree
propTests
  = testGroup "Projections"
    [ testProperty "invariant of direction" $
      let checkTree val root = mtValue root == val && all (checkTree val) (mtSubnodes root)
      in \x -> forAll (elements [Positive, Negative]) $ \dir ->
        let m = projection "id" id (eq x)
            tree = runIdentity $ m dir (Just $ Identity (x :: Int))
            treeView = prettyPrint defaultPPOptions tree
        in counterexample treeView $ checkTree (dir == Positive) tree
    ]

main :: IO ()
main = defaultMain propTests
