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

import Test.Matchers (MatchTree, match, projection, eq)
import Test.Matchers.QuickCheck ()
import Test.QuickCheck (quickCheckResult, isSuccess)
import System.Exit (exitSuccess, exitFailure)

prop_reverse_involutive :: [Int] -> MatchTree
prop_reverse_involutive xs = match (pReverse $ pReverse $ eq xs) xs
  where pReverse m = projection "reverse" reverse [m]

main :: IO ()
main = do
  ok <- isSuccess <$> quickCheckResult prop_reverse_involutive
  if ok
    then exitSuccess
    else exitFailure
