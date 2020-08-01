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
            Int 1 `testInference` Type.Int
        , testCase "Bool" $
            Bool 1 `testInference` Type.Bool
        ]
    , testGroup "functions"
        [ testCase "lambda" $
            inferExprType (Int 1, ()) 
                @?= 
                    Right (Int 1, Type.Int)
        , testCase "Bool" $
            inferExprType (Bool True, ()) @?= Right (Bool True, Type.Bool)
        ]
    ]

    where 
        testInference e t = inferExprType (e, ()) @?= Right (e, t)