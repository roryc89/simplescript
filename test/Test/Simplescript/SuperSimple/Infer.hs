{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Simplescript.SuperSimple.Infer where 

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import Data.Text as T
import Simplescript.SuperSimple.Expr
import Simplescript.SuperSimple.Infer
import qualified Simplescript.SuperSimple.Type as Type
import Text.Megaparsec.Pos
import           NeatInterpolation

tests :: TestTree
tests = testGroup "SuperSimple.Infer"
    [ testGroup "singles"
        [ testCase "integer" $
            inferExprType (Int 1, ()) @?= Right (Int 1, Type.Int)
        , testCase "integer" $
            inferExprType (Bool True, ()) @?= Right (Bool True, Type.Bool)
        ]
    ]