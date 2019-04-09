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

import Test.Matchers (Matcher, labeled, contramap, eqR)
import Test.Matchers.QuickCheck ()
import Test.QuickCheck (quickCheckResult, isSuccess)
import System.Exit (exitSuccess, exitFailure)

prop_reverse_involutive :: Matcher [Int]
prop_reverse_involutive = labeled "reverse.reverse involutive"
                          $ contramap (\x -> (reverse $ reverse x, x)) eqR

main :: IO ()
main = do
  ok <- isSuccess <$> quickCheckResult prop_reverse_involutive
  if ok
    then exitSuccess
    else exitFailure
