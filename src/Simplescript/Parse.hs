{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module Simplescript.Parse where 

import           Control.Applicative        (empty)
import           Control.Monad              (void)
import           Data.Char                  (isDigit, isAlpha, isAlphaNum)
import           Data.Text                  (unpack, Text)
import qualified Data.Text                  as T
import qualified Data.List                  as DL
import Data.Set as Set
import qualified Text.Read as R
import           Data.Void                  (Void)
import           NeatInterpolation          (text)
import qualified Text.Megaparsec            as Parsec
import           Text.Megaparsec            (choice, many, some, (<|>))
import qualified Text.Megaparsec.Char       as CharParser
import           Text.Megaparsec.Char       (char, string)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Simplescript.Token as Tok
import Simplescript.Token (WithPos(..), SToken)
import Simplescript.Ast

type Parser = Parsec.Parsec Void Tok.TokStream

-- sLex :: T.Text -> Either (Parsec.ParseErrorBundle T.Text Void) [TokenAndPosLine]
-- sLex = Parsec.runParser pLines "" 
sParseText :: Tok.TokenAndPosLine -> Either (Parsec.ParseErrorBundle T.Text Void) [StatementPos] 
sParseText = undefined 
-- STATEMENTS

pStatement :: Parser StatementPos
pStatement = choice
    [ Parsec.try pTypeAnnotation 
    , pVarDeclaration
    ]

pTypeAnnotation :: Parser StatementPos
pTypeAnnotation = undefined
  
pVarDeclaration :: Parser StatementPos
pVarDeclaration = undefined 

-- TYPES 
pType :: Parser TypePos
pType = choice
    [ pTypeIdentifier
    , pTypeApply
    , pTypeOperator
    ]

pTypeIdentifier :: Parser TypePos
pTypeIdentifier = undefined

pTypeApply :: Parser TypePos
pTypeApply = undefined

pTypeOperator :: Parser TypePos
pTypeOperator = undefined

-- EXPRESSIONS

pExpr :: Parser ExprPos
pExpr = choice 
    [ pVar
    , pParens
    , pLet
    , pLit
    ]

pVar :: Parser ExprPos
pVar = addPositions Var <$> pIdentifier 

pParens :: Parser ExprPos
pParens = do 
    open <- tokEq Tok.LParen
    inside <- pExpr 
    close <- tokEq Tok.RParen 
    return $
        Parens 
            (Positions (Tok.startPos open) (Tok.startPos close))
            inside

pLet :: Parser ExprPos
pLet = do 
    let_ <- tokEq (Tok.Keyword Tok.Let)
    statements <- many pStatement 
    in_ <- tokEq Tok.RParen
    e <- pExpr
    Let (Positions (Tok.startPos let_) (Tok.endPos in_)) statements
        <$> pExpr

pLit :: Parser ExprPos
pLit = Lit <$> pLiteral

pIdentifier :: Parser (WithPos Text) 
pIdentifier = tokNoErr (\case 
        Tok.Identifier n -> Just n
        _ -> Nothing
    )

-- LITERALS 

pLiteral :: Parser LiteralPos
pLiteral = choice 
    [ pNumberLit
    , pIntLit
    , pStringLit
    ]

pStringLit :: Parser LiteralPos
pStringLit = addPositions StringLit <$> pString 

pString :: Parser (WithPos Text)
pString = tokNoErr (\case 
        Tok.SString n -> Just n
        _ -> Nothing
    )

pIntLit :: Parser LiteralPos
pIntLit = addPositions IntLit <$> pInt 

pInt :: Parser (WithPos Int)
pInt = tokNoErr (\case 
        Tok.Int n -> Just n
        _ -> Nothing
    )

pNumberLit :: Parser LiteralPos
pNumberLit = addPositions NumberLit <$> pDouble 

pDouble :: Parser (WithPos Double)
pDouble = tokNoErr (\case 
        Tok.Number n -> Just n
        _ -> Nothing
    )

-- UTILS 

addPositions :: (Positions -> a -> b) -> WithPos a -> b
addPositions c WithPos{..} = c (Positions{..}) tokenVal

tokWPosNoErr :: (WithPos SToken -> Maybe a) -> Parser a
tokWPosNoErr f = Parsec.token f Set.empty

tokEq :: SToken -> Parser (WithPos SToken)
tokEq t = Parsec.satisfy ((==) t . tokenVal)

tokNoErr ::  (SToken -> Maybe a) -> Parser (WithPos a)
tokNoErr f = tokWPosNoErr 
    (\WithPos{..} -> 
        case f tokenVal of 
            Just a -> 
                Just $ WithPos startPos endPos tokenLength a 
            _ -> Nothing 
        )
