import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Simplescript.Lex as Lex
import qualified Test.Simplescript.Parse as Parse
import qualified Test.Simplescript.TypeCheck as TypeCheck
import Test.Tasty.Ingredients.Rerun


main = defaultMainWithRerun tests

tests :: TestTree
tests = testGroup "Tests"
  [ Lex.tests
  , Parse.tests
  , TypeCheck.tests
  ]