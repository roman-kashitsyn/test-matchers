import Test.HUnit
import Test.Matchers
import Test.Matchers.HUnit
import System.Exit
import Control.Monad (when)

myDiv :: Int -> Int -> Either String Int
myDiv _ 0 = Left "Division by zero"
myDiv x y = Right (x `quot` y)

good :: Test
good = TestCase $ myDiv 5 0 `shouldMatch` (isLeftWith $ hasInfix "zero")

main :: IO ()
main = do
  cs <- runTestTT $ good
  when (failures cs > 0) $
    exitFailure
