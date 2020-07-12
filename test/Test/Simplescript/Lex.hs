{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Test.Simplescript.Lex where 

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import Data.Text as T
import Simplescript.Lex (sLex)
import Text.Megaparsec.Pos
import Simplescript.Token
import           NeatInterpolation

tests :: TestTree
tests = testGroup "Lexer"
    [ testGroup "singles"
        [ testCase "integer" $
            runLexerWoPos "22" @?= Right [Line [Int 22] []]
        , testCase "number" $
            runLexerWoPos "55.2" @?= Right [Line [Number 55.2] []]
        , testCase "string" $
            runLexerWoPos "\"hello\"" @?= Right [Line [SString "hello"] []]
        , testCase "operator" $
            runLexerWoPos "<>" @?= Right [Line [Operator "<>"] []]
        , testCase "identifier" $
            runLexerWoPos "myIdent" @?= Right [Line [Identifier "myIdent"] []]
        ]
    , testGroup "multiline" 
        [ testCase "flat" $
            runLexerWoPos "a\n1" @?= 
                Right 
                  [ Line [Identifier "a"] []
                  , Line [Int 1] []
                  ]
        , testCase "indented" $
            runLexerWoPos "a\n    10.0" @?= 
                Right 
                  [ Line [Identifier "a"] [ Line [Number 10] []]
                  ]
        ]
    , testGroup "Round trip tests" $ fmap roundTripTest
      [ "([{}])" 
      , "([ {} ] )"
      , "a=1"
      , "a\nb"
      , "a\n\nb"
      , T.strip [text|
mul x y = x * y + 2

append a b = (a ++ b)
|]
      ]
    ]

    where 
        roundTripTest :: Text -> TestTree
        roundTripTest input = 
            testCase (T.unpack input) $ fmap showTokenAndPosLines (sLex input) @?= Right input

        runLexerWoPos = fmap (fmap removePositions) . sLex

        withPos line startCol len = 
             WithPos (at line startCol) (at line (startCol + len)) len
        at line col = SourcePos "" (mkPos line) (mkPos col)