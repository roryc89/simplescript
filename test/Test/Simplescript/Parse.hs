{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Simplescript.Parse where 

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import Data.Text as T
import Simplescript.Parse (parseText)
import Text.Megaparsec.Pos
import Simplescript.Ast
import           NeatInterpolation
import           Control.Monad              (void)

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


        ,  testCase "function literal" $
                parseTextWoPos "const = \\a, b => b" 
                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "const"
                            (Lit $ FunctionLit () [("a", ()), ("b", ())] (Var () "b"))
                        ]

        ,  testCase "list literal" $
                parseTextWoPos "l = [a, 2]" 
                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "l"
                            (Lit $ ListLit ()
                                [ Var () "a"
                                , Lit $ IntLit () 2
                                ]
                            )
                        ]

        ,  testCase "record literal" $
                parseTextWoPos "rec = {a = 1, b = x}" 
                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "rec"
                            (Lit $ RecordLit () 
                                [ ("a" , Lit $ IntLit () 1)
                                , ("b" , Var () "x")
                                ]
                            )
                        ]

        ,  testCase "if expression" $
                parseTextWoPos "x = if a then b else c" 
                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "x"
                            (If () (Var () "a") (Var () "b") (Var () "c")
                            )
                        ]


        ,  testCase "function application" $
                parseTextWoPos "applied = a b" 
                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "applied"
                            (Apply (Var () "a") (Var () "b"))
                        ]

        ,  testCase "type application" $
                parseTextWoPos "applied : A B" 
                    @?= Right 
                        [ TypeAnnotation 
                            ()
                            "applied"
                            (TypeApply (TypeIdentifier () "A") (TypeIdentifier () "B"))
                        ]

        ,  testCase "operator application" $
                parseTextWoPos "val = a + b" 
                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "val"
                            (Op () "+" (Var () "a") (Var () "b"))
                        ]
        ,  testCase "type declaration with no constructors" $
                parseTextWoPos "type MyT =" 
                    @?= Right 
                        [ TypeDeclaration () "MyT" [] []
                        ]

        ,  testCase "type declaration with 2 constructors" $
                parseTextWoPos "type MyT = A | B" 
                    @?= Right 
                        [ TypeDeclaration () "MyT" [] [Ctr () "A" [], Ctr () "B" []]
                        ]


        ,  testCase "type operator application" $
                parseTextWoPos "val : a + b" 
                    @?= Right 
                        [ TypeAnnotation 
                            ()
                            "val"
                            (TypeOp () "+" (TypeIdentifier () "a") (TypeIdentifier () "b"))
                        ]
        ,  
            let 
                result  = 
                    Right 
                        [ VarDeclaration 
                            ()
                            "val"
                            ( Let 
                                () 
                                [ VarDeclaration () "a" (Lit $ IntLit () 1)]
                                ( Var () "b")
                            )
                        ]
            in
            testGroup "single let expr values" 

            [ testCase "multiple line with indent"  $ parseTextWoPos [text|
val =
    let 
        a = 1
    in 
        b
                |]  @?= result
            , testCase "multiple line without indent (and extra space)"  $ parseTextWoPos [text|
val =
    let 
        a = 1
    in 

    b
                |]  @?= result
            , testCase "single line"  $ parseTextWoPos [text|
val = let a = 1 in b
                |]  @?= result

            ]
                   
        ]
    , testGroup "multiline" 
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
        ,  testCase "string value indented" $
                parseText "val = \n    \"a\"" 
                    @?= Right 
                        [ VarDeclaration 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            (Lit $ StringLit (Positions (sp 2 5)  (sp 2 8)) "a" )
                        ]

        ,  testCase "multiple values indented" $
                parseText [text|
val = 
    "a"
new = 
    123
                |]
                    @?= Right 
                        [ VarDeclaration 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            (Lit $ StringLit (Positions (sp 2 5)  (sp 2 8)) "a" )
                        , VarDeclaration 
                            (Positions (sp 3 1)  (sp 3 6))
                            "new"
                            (Lit $ IntLit (Positions (sp 4 5)  (sp 4 8)) 123 )
                        ]

        ,  testCase "mixed types values indented" $
                parseText [text|
val : 
    MyT
val = 
    123
                |]
                    @?= Right 
                        [ TypeAnnotation 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            (TypeIdentifier (Positions (sp 2 5)  (sp 2 8)) "MyT" )
                        , VarDeclaration 
                            (Positions (sp 3 1)  (sp 3 6))
                            "val"
                            (Lit $ IntLit (Positions (sp 4 5)  (sp 4 8)) 123 )
                        ]
        ,  testCase "list literal vertically aligned" $
                parseTextWoPos [text|
l = 
    [ a
    , 2
    ]
                |]
                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "l"
                            (Lit $ ListLit ()
                                [ Var () "a"
                                , Lit $ IntLit () 2
                                ]
                            )
                        ]


        ,  testCase "record literal vertically aligned" $
                parseTextWoPos [text|
rec = 
    { a = 1
    , b = x
    }
                |]
                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "rec"
                            (Lit $ RecordLit () 
                                [ ("a" , Lit $ IntLit () 1)
                                , ("b" , Var () "x")
                                ]
                            )
                        ]


        ,  testCase "function application vertically aligned" $
                parseTextWoPos [text|
applied 
    = 
    a 
    b
                |]

                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "applied"
                            (Apply (Var () "a") (Var () "b"))
                        ]

        ,  testCase "type constructors vertically aligned" $
                parseTextWoPos [text|
type MyT 
    = A 
    | B
                |]
                    @?= Right 
                        [ TypeDeclaration () "MyT" [] [Ctr () "A" [], Ctr () "B" []] ]
        ,  
            let 
                result  = 
                    Right 
                        [ VarDeclaration 
                            ()
                            "val"
                            ( Let 
                                () 
                                [ TypeAnnotation () "a" (TypeIdentifier () "Int")
                                , VarDeclaration () "a" (Lit $ IntLit () 1)
                                , VarDeclaration () "b" (Lit $ StringLit () "string")
                                ]
                                ( Var () "c")
                            )
                        ]
            in
            testGroup "multiple let expr values" 
                [ testCase "multiple line with indent"  $ parseTextWoPos [text|
val =
    let 
        a : Int
        a = 1
        b = "string"
    in 
        c
                |]  @?= result
                , testCase "multiple line without indent (and extra space)"  $ parseTextWoPos [text|
val =
    let 
        a : Int
        a = 1
        b = "string"
    in 

    c
                |]  @?= result
            ]
        ]
    ]

    where 
        sp start end = SourcePos "" (mkPos start) (mkPos end)
         
        parseTextWoPos = fmap (fmap void) . parseText