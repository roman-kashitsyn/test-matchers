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
import Prelude hiding (lex)
import Test.Matchers
import Test.Matchers.LexShow
  ( LexResult(Full, Partial)
  , TokType(..)
  , Token
  , lexShow
  , tokenText
  , tokenType
  )
import Test.QuickCheck
  ( Arbitrary(arbitrary, shrink)
  , Property
  , counterexample
  , discard
  , elements
  , forAll
  , frequency
  , oneof
  , sized
  , vectorOf
  , (.&.)
  , (===)
  )
import Test.Tasty (TestTree, adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckMaxSize(..), testProperty)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Text.Read.Lex (Lexeme(..), lex)

data PTree
  = PEmpty
  | PInt Int
  | PDouble Double
  | PChar Char
  | PString String
  | PList [PTree]
  | PStruct { pName :: String, pChild :: PTree }
  -- We use a fancy symbol here to check that operators are lexed
  -- correctly.
  | PTree :|: PTree
  deriving (Show, Eq)

instance Arbitrary PTree where
  arbitrary = sized arb'
    where arb' n | n <= 0  = pure PEmpty
          arb' 1 = frequency [ (10, PInt <$> arbitrary)
                             , (10, PDouble <$> arbitrary)
                             , (10, PChar <$> arbitrary)
                             , (10, PString <$> arbitrary)
                             , (1, pure $ PEmpty :|: PEmpty)
                             ]
          arb' k = oneof [ (:|:) <$> arb' (k `quot` 2) <*> arb' (k `quot` 2)
                         , PList <$> vectorOf k (arb' 1)
                         , PStruct <$> arbitrary <*> arb' (k - 1)
                         ]
  shrink (l :|: r) = [PEmpty, l, r] ++ [l' :|: r' | (l', r') <- shrink (l, r)]
  shrink (PList l) = PEmpty : map PList (shrink l)
  shrink (PStruct n c) = [PStruct n' c' | (n', c') <- shrink (n, c)]
  shrink _ = []

untilP :: (a -> Bool) -> ReadP a -> ReadP [a]
untilP p parser = do
  v <- parser
  if p v
    then return []
    else (v:) <$> untilP p parser

tokenize :: String -> [Lexeme]
tokenize s = case readP_to_S (untilP (== EOF) lex) s of
               [(lexems, "")] -> lexems
               _ -> error $ "failed to lex " ++ s

sameTokens :: [Token] -> [Lexeme] -> Bool
sameTokens xs = check (filter (\t -> tokenType t /= TokSpace) xs)
  where check [] [] = True
        check (t:ts) (l:ls) = let tokIs s = T.pack s == tokenText t
                              in case (tokenType t, l) of
                                   (TokNumber, Number n) -> check ts ls
                                   (TokChar, Char c) -> tokIs (show c) && check ts ls
                                   (TokString, String s) -> tokIs (show s) && check ts ls
                                   (TokIdent, Ident s) -> tokIs s && check ts ls
                                   (TokSymbol, Symbol s) -> tokIs s && check ts ls
                                   (TokPunct, Punc s) -> tokIs s && check ts ls
                                   _ -> False
        check _ _ = False

prop_proj_dir_invariant :: Int -> Property
prop_proj_dir_invariant =
  let checkTree val root = mtValue root == val && all (checkTree val) (mtSubnodes root)
  in \x -> forAll (elements [Positive, Negative]) $ \dir ->
    let m = projection "id" id (eq x)
        tree = match (case dir of
                         Positive -> m
                         Negative -> negationOf m) x
        treeView = prettyPrint defaultPPOptions tree
    in counterexample treeView $ checkTree (dir == Positive) tree

formString :: [Token] -> String -> Property
formString ts s = T.concat (map tokenText ts) === T.pack s

prop_lex_keeps_formatting :: PTree -> Property
prop_lex_keeps_formatting t = let showed = show t
                              in case lexShow maxBound showed of
                                   Full tokens -> tokens `formString` showed
                                   Partial _ -> discard

prop_lex_agrees_with_std_lex :: PTree -> Property
prop_lex_agrees_with_std_lex t =
  let showed = show t
      tokenized = tokenize showed
  in case lexShow maxBound showed of
       Full tokens -> counterexample (show (tokens, tokenized)) $ sameTokens tokens tokenized
       Partial _ -> discard

prop_lex_handles_garbage :: String -> Property
prop_lex_handles_garbage garbage =
  case lexShow maxBound garbage of
    Full tokens -> tokens `formString` garbage
    Partial _ -> discard

parsesAsOneToken :: (Show a) => TokType -> a -> Property
parsesAsOneToken tok x =
  let showed = show x
  in case lexShow maxBound showed of
    Full tokens -> length tokens === 1 .&. tokens `formString` showed .&. tokenType (head tokens) === tok
    Partial _ -> discard

prop_lex_can_parse_chars :: Char -> Property
prop_lex_can_parse_chars = parsesAsOneToken TokChar

prop_lex_can_parse_strings :: String -> Property
prop_lex_can_parse_strings = parsesAsOneToken TokString

propTests :: TestTree
propTests
  = testGroup "Properties"
    [ testGroup "Projections"
      [ testProperty "invariant of direction" prop_proj_dir_invariant
      ]
    , adjustOption (`min` QuickCheckMaxSize 16) $
      testGroup "Pretty Printing"
      [ testProperty "Lexing preserves formatting" prop_lex_keeps_formatting
      , testProperty "Lexing agrees with Text.Read.Lex" prop_lex_agrees_with_std_lex
      , testProperty "Lexing can parse chars" prop_lex_can_parse_chars
      , testProperty "Lexing can parse strings" prop_lex_can_parse_strings
      , testProperty "Lexing can handle garbage" prop_lex_handles_garbage
      ]
    ]

main :: IO ()
main = defaultMain propTests
