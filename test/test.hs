import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Simplescript.Lex as Lex
import qualified Test.Simplescript.Parse as Parse

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ Lex.tests
  , Parse.tests
  ]