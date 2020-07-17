{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Simplescript.TypeCheck where 

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import qualified Data.Text as T
import Simplescript.TypeCheck (checkModuleText)
import Simplescript.Error (SimpleError(..), TypeCheckError(..))
import Simplescript.TypeChecked
import qualified Simplescript.Ast as Ast
import Text.Megaparsec.Pos
import           NeatInterpolation
import           Control.Monad              (void)
import Data.Bifunctor (first)

tests :: TestTree
tests = testGroup "TypeCheck"
    [ testGroup "passing singles" 
        [ testCase "integer value without annotation" $ do 
            "val = 22"
                `passesCheck`
                    [ Val (Int 22) (Concrete "Int")
                    ]

        , testCase "string value without annotation" $ do 
            "val = \"str\""
                `passesCheck`
                    [ Val (String "str") (Concrete "Int")
                    ]

        , testCase "integer value with annotation" $ do 
            "val : Int\nval = 22"
                `passesCheck`
                    [ Val (Int 22) (Concrete "Int")
                    ]
        ]

    , testGroup "failing singles" 
        [ testCase "integer value with annotation" $ do 
            "val : String\nval = 22"
                `failsCheck`
                    [ TypesDoNotUnify 
                        (Ast.TypeIdentifier () "String")
                        (Ast.TypeIdentifier () "Int")
                    ]
        ]
    ]

    where 
        passesCheck input expected = do 
            result <- checkModuleText input
            result @?= Right expected

        failsCheck :: Text -> [TypeCheckError ()] -> IO ()
        failsCheck input errs = do 
            result <- checkModuleText input
            first void result @?= Left (TypeCheckError errs)
