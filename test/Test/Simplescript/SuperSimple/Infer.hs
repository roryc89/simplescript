{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Simplescript.SuperSimple.Infer where 

import qualified Data.Map as Map
import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import Simplescript.SuperSimple.Expr
import Simplescript.SuperSimple.Infer
import qualified Simplescript.SuperSimple.Type as T
import Text.Megaparsec.Pos
import           NeatInterpolation

tests :: TestTree
tests = testGroup "SuperSimple.Infer"
    [ testGroup "Singles"
        [ testCase "Integer" $
            Int 1 `testInference` T.Int
        , testCase "Bool" $
            Bool True `testInference` T.Bool
        ]
    , testGroup "Lambdas" $
        [ testCase "Identity" $ 
            Lambda "a" (Var "a", ()) 
                `testInference`
                    T.Function (T.Id 0) (T.Id 0)
        , testCase "Const" $ 
            Let [ ("b", E (Int 1, () )) ]
             (Lambda "a" (Var "b", ()), ())
                `testInference`
                    T.Function (T.Id 0) (T.Int)
        , testCase "Let const" $ 
            Let [ ("b", E (Int 1, () )) ]
             (Lambda "a" (Var "b", ()), ())
                `testInference`
                    T.Function (T.Id 0) (T.Int)
        ]
    , testGroup "Custom types" $

        [ testCase "Let const" $ 
            Let [ ("T", T [Ctr "C" []]) ]
             (Lambda "a" (Var "C", ()), ())
                `testInference`
                    T.Function (T.Id 1) (T.UserDefined "C" 0 )
        ]
    ]

    where 
        -- let_ bs = Let bs)
        testInference e t = fmap snd (inferExprTypeStandalone (e, ())) @?= Right t