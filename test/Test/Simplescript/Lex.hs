{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Simplescript.Lex where 

import Test.Tasty
import Test.Tasty.HUnit
import Simplescript.Lex (sLex)
import Text.Megaparsec.Pos
import Simplescript.Token
import           NeatInterpolation

tests :: TestTree
tests = testGroup "Lexer"
    [ testGroup "singles"
        [ testCase "integer" $
            runLexerWoPos "22" @?= Right [Line "22" [Int 22] []]
        , testCase "number" $
            runLexerWoPos "55.2" @?= Right [Line "55.2" [Number 55.2] []]
        , testCase "string" $
            runLexerWoPos "\"hello\"" @?= Right [Line "\"hello\"" [SString "hello"] []]
        , testCase "operator" $
            runLexerWoPos "<>" @?= Right [Line "<>" [Operator "<>"] []]
        , testCase "identifier" $
            runLexerWoPos "myIdent" @?= Right [Line "myIdent" [Identifier "myIdent"] []]
        ]
--     , testGroup "simple multiple without whitespace"
--         [ testCase "brackets" $
--             runLexerWoPos "([{}])" 
--                 @?= Right 
--                     [ LParen
--                     , LSquareBracket 
--                     , LBrace 
                    
--                     , RBrace 
--                     , RSquareBracket 
--                     , RParen 
--                     ]
--         , testCase "assignment" $
--             runLexerWoPos "a=1," 
--                 @?= Right 
--                     [ Identifier "a"
--                     , Assign  
--                     , Int 1 
--                     , Comma
--                     ]
--         ]
--     , testGroup "simple multiple with whitespace"
--         [ testCase "brackets" $
--             runLexerWoPos "([ {} ] ) " 
--                 @?= Right 
--                     [ LParen
--                     , LSquareBracket 
--                     , LBrace 
--                     , RBrace 
--                     , RSquareBracket 
--                     , RParen 
--                     ]
--         , testCase "assignment" $
--             runLexerWoPos "a = 1 :" 
--                 @?= Right 
--                     [ Identifier "a"
--                     , Assign  
--                     , Int 1
--                     , Colon
--                     ]
--         ]
--     , testCase "longer multiline lexing" $
--         runLexerWoPos [text|
-- mul x y = x * y + 2

-- append a b = (a ++ b)
-- |]
--             @?= Right 
--                 [ Identifier "mul"
--                 , Identifier "x" 
--                 , Identifier "y"
--                 , Assign
--                 , Identifier "x" 
--                 , Operator "*" 
--                 , Identifier "y"
--                 , Operator "+" 
--                 , Int 2 
--                 , Identifier "append"
--                 , Identifier "a" 
--                 , Identifier "b"
--                 , Assign
--                 , LParen 
--                 , Identifier "a" 
--                 , Operator "++" 
--                 , Identifier "b"
--                 , RParen 
--                 ]
--     , testGroup "with position"
--         [ testCase "brackets" $
--             runLexer "([{}])" 
--                 @?= Right 
--                     [ withPos 1 1 1 LParen
--                     , withPos 1 2 1 LSquareBracket 
--                     , withPos 1 3 1 LBrace 
--                     , withPos 1 4 1 RBrace 
--                     , withPos 1 5 1 RSquareBracket 
--                     , withPos 1 6 1 RParen 
--                     ]
--     , testCase "longer multiline lexing" $
--         runLexer [text|
-- mul x y = x * y + 2

-- append a b = (a ++ b)

-- |]
--             @?= Right 
--                 [ withPos 1 1 3 $ Identifier "mul"
--                 , withPos 1 5 1 $ Identifier "x" 
--                 , withPos 1 7 1 $ Identifier "y"
--                 , withPos 1 9 1 Assign
--                 , withPos 1 11 1 $ Identifier "x" 
--                 , withPos 1 13 1 $ Operator "*" 
--                 , withPos 1 15 1 $ Identifier "y"
--                 , withPos 1 17 1 $ Operator "+" 
--                 , withPos 1 19 1 $ Int 2 
--                 , withPos 3 1 6 $ Identifier "append"
--                 , withPos 3 8 1 $ Identifier "a" 
--                 , withPos 3 10 1 $ Identifier "b"
--                 , withPos 3 12 1 Assign
--                 , withPos 3 14 1 LParen 
--                 , withPos 3 15 1 $ Identifier "a" 
--                 , withPos 3 17 2 $ Operator "++" 
--                 , withPos 3 20 1 $ Identifier "b"
--                 , withPos 3 21 1 RParen 
--                 ]
--         ]
    ]

    where 
        -- -- runLexerWoPos :: Text -> Either (Parsec.ParseErrorBundle Text Void) [SToken]
        runLexerWoPos = fmap (fmap removePositions) . sLex
        withPos line startCol len = 
             WithPos (at line startCol) (at line (startCol + len)) len
        at line col = SourcePos "" (mkPos line) (mkPos col)