{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Simplescript.Parse where 

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import Data.Text as T
import Simplescript.Parse (parseText)
import Text.Megaparsec.Pos
import Simplescript.Token
import Simplescript.Ast
import           NeatInterpolation

tests :: TestTree
tests = testGroup "Parse"
    [ testGroup "singles" 
        [  testCase "integer value" $
                parseText "val = 22" 
                    @?= Right 
                        [ VarDeclaration 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            (Lit $ IntLit (Positions (sp 1 7)  (sp 1 9)) 22 )

                        ]
        ,  testCase "string value" $
                parseText "val = \"a\"" 
                    @?= Right 
                        [ VarDeclaration 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            (Lit $ StringLit (Positions (sp 1 7)  (sp 1 10)) "a" )
                        ]
        ,  testCase "number and parens value" $
                parseText "val = (10.0)" 
                    @?= Right 
                        [ VarDeclaration 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            ( Parens 
                                (Positions (sp 1 7)  (sp 1 12)) 
                                (Lit $ NumberLit (Positions (sp 1 8)  (sp 1 12)) 10.0 ))
                        ]
        ,  testCase "type annotation" $
                parseText "val : Int" 
                    @?= Right 
                        [ TypeAnnotation 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            (TypeIdentifier (Positions (sp 1 7)  (sp 1 10)) "Int" )
                        ]
        ]
    , testGroup "muliline" 
        [  testCase "integer values" $
                parseText "val = 22\nval2 = 42" 
                    @?= Right 
                        [ VarDeclaration 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            (Lit $ IntLit (Positions (sp 1 7)  (sp 1 9)) 22 )
                        , VarDeclaration 
                            (Positions (sp 2 1)  (sp 2 7))
                            "val2"
                            (Lit $ IntLit (Positions (sp 2 8)  (sp 2 10)) 42)

                        ]
        ,  testCase "string value" $
                parseText "val = \"a\"" 
                    @?= Right 
                        [ VarDeclaration 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            (Lit $ StringLit (Positions (sp 1 7)  (sp 1 10)) "a" )
                        ]
        ,  testCase "type annotation" $
                parseText "val : Int" 
                    @?= Right 
                        [ TypeAnnotation 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            (TypeIdentifier (Positions (sp 1 7)  (sp 1 10)) "Int" )
                        ]
        ]
    ]

    where 
        sp start end = SourcePos "" (mkPos start) (mkPos end)