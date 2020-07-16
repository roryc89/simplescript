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
                            (Lit (IntLit (Positions (sp 1 7)  (sp 1 9)) 22 ) Nothing)

                        ]
        ,  testCase "string value" $
                parseText "val = \"a\"" 
                    @?= Right 
                        [ VarDeclaration 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            (Lit (StringLit (Positions (sp 1 7)  (sp 1 10)) "a" ) Nothing)
                        ]
        ,  testCase "number and parens value" $
                parseText "val = (10.0)" 
                    @?= Right 
                        [ VarDeclaration 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            ( Parens 
                                (Positions (sp 1 7)  (sp 1 12)) 
                                (Lit (NumberLit (Positions (sp 1 8)  (sp 1 12)) 10.0 ) Nothing)
                                Nothing
                            )
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
                            (Lit (FunctionLit () [VarDes () "a" [] Nothing, VarDes () "b" [] Nothing] (Var () "b" Nothing)) Nothing)
                        ]

        ,  testCase "list literal" $
                parseTextWoPos "l = [a, 2]" 
                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "l"
                            (Lit 
                                ( ListLit ()
                                    [ Var () "a" Nothing
                                    , Lit (IntLit () 2) Nothing
                                    ]
                                )
                                Nothing
                            )
                        ]

        ,  testCase "record literal" $
                parseTextWoPos "rec = {a = 1, b = x}" 
                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "rec"
                            (Lit 
                                ( RecordLit () 
                                    [ ("a" , Lit (IntLit () 1) Nothing)
                                    , ("b" , Var () "x" Nothing)
                                    ]
                                )
                                Nothing
                            )
                        ]

        ,  testCase "if expression" $
                parseTextWoPos "x = if a then b else c" 
                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "x"
                            (If () (Var () "a" Nothing) (Var () "b" Nothing) (Var () "c" Nothing)
                            )
                        ]


        ,  testCase "function application" $
                parseTextWoPos "applied = a b" 
                    @?= Right 
                        [ VarDeclaration 
                            ()
                            "applied"
                            (Apply (Var () "a" Nothing) (Var () "b" Nothing))
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
                            (Op () "+" (Var () "a" Nothing) (Var () "b" Nothing))
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
                                [ VarDeclaration () "a" (Lit (IntLit () 1) Nothing) ]
                                ( Var () "b" Nothing)
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
                            (Lit (IntLit (Positions (sp 1 7)  (sp 1 9)) 22 ) Nothing)
                        , VarDeclaration 
                            (Positions (sp 2 1)  (sp 2 7))
                            "val2"
                            (Lit (IntLit (Positions (sp 2 8)  (sp 2 10)) 42) Nothing)

                        ]
        ,  testCase "string value indented" $
                parseText "val = \n    \"a\"" 
                    @?= Right 
                        [ VarDeclaration 
                            (Positions (sp 1 1)  (sp 1 6))
                            "val"
                            (Lit  (StringLit (Positions (sp 2 5)  (sp 2 8)) "a" ) Nothing)
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
                            (Lit  (StringLit (Positions (sp 2 5)  (sp 2 8)) "a" ) Nothing)
                        , VarDeclaration 
                            (Positions (sp 3 1)  (sp 3 6))
                            "new"
                            (Lit (IntLit (Positions (sp 4 5)  (sp 4 8)) 123 ) Nothing)
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
                            (Lit (IntLit (Positions (sp 4 5)  (sp 4 8)) 123 ) Nothing)
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
                            (Lit 
                                ( ListLit ()
                                    [ Var () "a" Nothing
                                    , Lit (IntLit () 2) Nothing
                                    ]
                                )
                                Nothing
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
                            (Lit 
                                ( RecordLit () 
                                  [ ("a" , Lit (IntLit () 1) Nothing)
                                  , ("b" , Var () "x" Nothing)
                                  ]
                                )
                                Nothing
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
                            (Apply (Var () "a" Nothing) (Var () "b" Nothing) )
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
                                , VarDeclaration () "a" (Lit (IntLit () 1) Nothing)
                                , VarDeclaration () "b" (Lit (StringLit () "string") Nothing)
                                ]
                                ( Var () "c" Nothing)
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
        , testGroup "case expressions and destructuring" 
            [  testCase "single branch" $
                    parseTextWoPos [text|
val = case a of 
    { b => c }
                |]
                        @?= Right 
                            [ VarDeclaration  () "val"
                                $ Case () (Var () "a" Nothing)
                                [ (VarDes () "b" [] Nothing, Var () "c" Nothing)
                                ]
                                Nothing
                            ]
            , testCase "two branches with int destructuring" $
                    parseTextWoPos [text|
val = case a of 
    { 0 => c
    , 1 => d
    }
                |]
                        @?= Right 
                            [ VarDeclaration  () "val"
                                $ Case () (Var () "a" Nothing)
                                [ (IntDes () 0 Nothing, Var () "c" Nothing)
                                , (IntDes () 1 Nothing, Var () "d" Nothing)
                                ]
                                Nothing
                            ]

            , testCase "record destructuring" $
                    parseTextWoPos [text|
val = case a of 
    {   { b = 1
        , c = "c"
        , x
        } => d
    }
                |]
                        @?= Right 
                            [ VarDeclaration  () "val"
                                $ Case () (Var () "a" Nothing)
                                [  ( RecordDes () 
                                        [ ("b", () , Just $ IntDes () 1 Nothing)
                                        , ("c", () , Just $ StringDes () "c" Nothing)
                                        , ("x", () , Nothing)
                                        ]
                                        Nothing
                                    , Var () "d" Nothing
                                    )
                                ]
                                Nothing
                            ]
            , testCase "list destructuring" $
                    parseTextWoPos [text|
val = case a of 
    { [1.0, 10.0] => c}
                |]
                        @?= Right 
                            [ VarDeclaration  () "val"
                                $ Case () (Var () "a" Nothing)
                                [ (ListDes () [NumberDes () 1.0 Nothing, NumberDes () 10.0 Nothing] Nothing, Var () "c" Nothing)
                                ]
                                Nothing
                            ]
            , testCase "constructor and string destructuring" $
                    parseTextWoPos [text|
val = case a of 
    { Just "a" => c
    }
                |]
                        @?= Right 
                            [ VarDeclaration  () "val"
                                $ Case () (Var () "a" Nothing)
                                [ (VarDes () "Just" [StringDes () "a" Nothing] Nothing, Var () "c" Nothing)
                                ]
                                Nothing
                            ]
            , testCase "multiple case expressions" $
                    parseTextWoPos [text|
val = case a of 
    {b => c}

val = case a of 
    { 0 => c
    , 1 => d
    }
                |]
                        @?= Right 
                            [ VarDeclaration  () "val"
                                $ Case () (Var () "a" Nothing)
                                [ (VarDes () "b" [] Nothing, Var () "c" Nothing)
                                ]
                                Nothing
                            , VarDeclaration  () "val"
                                $ Case () (Var () "a" Nothing)
                                [ (IntDes () 0 Nothing, Var () "c" Nothing)
                                , (IntDes () 1 Nothing, Var () "d" Nothing)
                                ]
                                Nothing
                            ]
            ]
        ]
    ]

    where 
        sp start end = SourcePos "" (mkPos start) (mkPos end)
         
        parseTextWoPos = fmap (fmap void) . parseText