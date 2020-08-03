import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Simplescript.Lex as Lex
import qualified Test.Simplescript.SuperSimple.Infer as SuperSimple.Infer
-- import qualified Test.Simplescript.Parser as Parser


main = defaultMainWithRerun tests

tests :: TestTree
tests = testGroup "Tests"
  [ Lex.tests
  , SuperSimple.Infer.tests
  ]