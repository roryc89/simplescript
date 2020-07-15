{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}


module Simplescript.Parse (parseText) where 

import           Control.Applicative        (empty)
import           Control.Monad              (void)
import           Data.Char                  (isDigit, isAlpha, isAlphaNum)
import           Data.Text                  (unpack, Text)
import qualified Data.Text                  as T
import qualified Data.List                  as DL
import qualified Data.Set as Set
import qualified Text.Read as R
import           Data.Void                  (Void)
import           NeatInterpolation          (text)
import qualified Text.Megaparsec            as Parsec
import           Text.Megaparsec            (choice, many, some, (<|>))
import qualified Text.Megaparsec.Char       as CharParser
import           Text.Megaparsec.Char       (char, string)
import           Text.Megaparsec.Debug
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Simplescript.Token as Tok
import qualified Simplescript.Lex as Lex
import Simplescript.Token (WithPos(..), SToken)
import Simplescript.Ast
import Simplescript.Error (ParseOrLexError(..))
import Simplescript.Parse.Operator (operatorTable)
import Simplescript.Parse.TypeOperator (typeOperatorTable)
import Control.Monad.Combinators.Expr (makeExprParser)

type Parser = Parsec.Parsec Void Tok.TokStream

parseText :: T.Text -> Either ParseOrLexError [StatementPos]
parseText input = case Lex.sLex input of 
    Left lexErr -> Left $ LexError lexErr
    Right toks -> 
        case sParseTopLevelStatements $ Tok.TokStream $ Tok.flattenLines toks of 
            Left parseErr -> Left $ ParseError parseErr 
            Right result -> Right result


sParseTopLevelStatements :: Tok.TokStream -> Either (Parsec.ParseErrorBundle Tok.TokStream Void) [StatementPos] 
sParseTopLevelStatements = Parsec.runParser pStatementsTopLevel "" 

-- STATEMENTS

pStatementsTopLevel :: Parser [StatementPos]
pStatementsTopLevel = Parsec.manyTill (lexemeNewAndIndent pStatement) Parsec.eof

pStatement :: Parser StatementPos
pStatement = choice
    [ pTypeDeclaration
    , Parsec.try pTypeAnnotation
    , pVarDeclaration
    ]

pTypeDeclaration :: Parser StatementPos
pTypeDeclaration = do 
    keyword <- tokEq (Tok.Keyword Tok.Type)
    name <- pIdentifier
    args <- many pArg
    assign <- tokEq Tok.Assign 
    TypeDeclaration 
        (Positions (Tok.startPos name) (Tok.endPos assign))
        (tokenVal name)
        args
        <$> Parsec.sepBy pCtr (tokEq Tok.Pipe)


pTypeAnnotation :: Parser StatementPos
pTypeAnnotation = do 
    name <- pIdentifier
    colon <- tokEq Tok.Colon
    TypeAnnotation 
        (Positions (Tok.startPos name) (Tok.endPos colon))
        (tokenVal name)
        <$> pType

  
pVarDeclaration :: Parser StatementPos
pVarDeclaration = do 
    name <- pIdentifier
    assign <- tokEq Tok.Assign 
    VarDeclaration 
        (Positions (Tok.startPos name) (Tok.endPos assign))
        (tokenVal name)
        <$> pExpr

-- TYPE CONSTRUCTORS 

pCtr :: Parser CtrPos
pCtr = do 
    name <- pIdentifier
    Ctr (btwWithPos name name) (tokenVal name) <$> many pType


-- TYPES 

pType :: Parser TypePos
pType = makeExprParser pTypeTerms typeOperatorTable

pTypeTerms :: Parser TypePos
pTypeTerms = choice
    [ pTypeIdentifier
    ]

pTypeIdentifier :: Parser TypePos
pTypeIdentifier = addPositions TypeIdentifier <$> pIdentifier 


-- EXPRESSIONS

pExpr :: Parser ExprPos
pExpr = makeExprParser pTerms operatorTable

pTerms :: Parser ExprPos
pTerms = choice 
    [ pLit
    , pVar
    , pParens
    , pLet
    , pIf
    , pCase
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
    let_ <- lexemeNewAndIndent (tokEq (Tok.Keyword Tok.Let))

    statements <-  
        Parsec.manyTill (lexemeNewAndIndent pStatement) pIn

    newAndIndents

    Let (Positions (Tok.startPos let_) (Tok.endPos let_)) statements
        <$> pExpr

pIf :: Parser ExprPos
pIf = do
    ifK <- tokEq (Tok.Keyword Tok.If)
    if_ <- pExpr
    thenK <- tokEq (Tok.Keyword Tok.Then)
    then_ <- pExpr 
    elseK <- tokEq (Tok.Keyword Tok.Else)
    else_ <- pExpr 
    end <- Parsec.getSourcePos 
    return $ If 
        (Positions (Tok.startPos ifK) end) 
        if_
        then_
        else_

pCase :: Parser ExprPos
pCase = do
    caseK <- dbg "caseK" $ tokEq (Tok.Keyword Tok.Case)
    case_ <- dbg "case_" $ pExpr
    tokEq $ Tok.Keyword Tok.Of
    openBr <- dbg "openBr" $ tokEq Tok.LBrace

    branches <- dbg "branches" $  
        -- Parsec.sepBy pBranch (tokEq Tok.Comma)
        Parsec.sepBy (lexemeNewAndIndent pBranch) (tokEq Tok.Comma)

    closeBr <- dbg "closeBr" $ tokEq Tok.RBrace

    end <- dbg "end" $ Parsec.getSourcePos 
    return 
        $ Case
            (Positions (Tok.startPos caseK) end)
            case_
            branches

pBranch :: Parser (DestructuredPos, ExprPos)
pBranch = do 
    if_ <- pDestructured
    tokEq Tok.Arrow
    then_ <- pExpr
    return (if_, then_)
    

pIn :: Parser (WithPos SToken)
pIn =  tokEq (Tok.Keyword Tok.In)

pLit :: Parser ExprPos
pLit = Lit <$> pLiteral

pIdentifier :: Parser (WithPos Text) 
pIdentifier = tokNoErr (\case 
        Tok.Identifier n -> Just n
        _ -> Nothing
    )

-- DESTRUCTURED

pDestructured :: Parser DestructuredPos
pDestructured = choice 
  [ addPositions IntDes <$> pInt
  , addPositions NumberDes <$> pDouble
  , addPositions StringDes <$> pString
  , pVarDes
  ] 

pVarDes :: Parser DestructuredPos
pVarDes = do 
    name <- pIdentifier
    args <- many pDestructured
    end <- Parsec.getSourcePos 
    return 
        $ VarDes
            (Positions (Tok.startPos name) end)
            (tokenVal name)
            args

-- LITERALS 

pLiteral :: Parser LiteralPos
pLiteral = choice 
    [ pNumberLit
    , pIntLit
    , pStringLit
    , pListLit
    , pRecordLit
    , pFunctionLit
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

pListLit :: Parser LiteralPos
pListLit = do 
    open <- tokEq Tok.LSquareBracket
    items <- Parsec.sepBy pExpr (tokEq Tok.Comma)
    close <- tokEq Tok.RSquareBracket
    return $ ListLit (btwWithPos open close) items

pRecordLit :: Parser LiteralPos
pRecordLit = do 
    open <- tokEq Tok.LBrace
    items <- Parsec.sepBy (pRecordKeyVal pExpr) (tokEq Tok.Comma)
    close <- tokEq Tok.RBrace
    return $ RecordLit (btwWithPos open close) items

pRecordKeyVal :: Parser a -> Parser (T.Text, a)
pRecordKeyVal p = do 
    key <- pIdentifier
    void $ tokEq Tok.Assign
    val <- p
    return (tokenVal key, val)

pFunctionLit :: Parser LiteralPos
pFunctionLit = do 
    slash <- tokEq Tok.Backslash 
    args <- Parsec.sepBy pArg (tokEq Tok.Comma)
    arrow <- tokEq Tok.Arrow
    expr <- pExpr
    end <- Parsec.getSourcePos 
    return $ FunctionLit (Positions (Tok.startPos slash) end) args expr

pArg :: Parser (Text, Positions)
pArg = go <$> pIdentifier
    where 
        go WithPos{..} = (tokenVal, Positions startPos endPos)

-- LEXEME 

newAndIndents :: Parser ()
newAndIndents = Lexer.space (void takeNewAndIndent) empty empty

takeNewAndIndent :: Parser [WithPos SToken]
takeNewAndIndent = Parsec.takeWhile1P Nothing (pred . tokenVal)
  where
    pred t = t == Tok.Newline || t == Tok.Newline

lexemeNewAndIndent :: Parser a -> Parser a
lexemeNewAndIndent = Lexer.lexeme newAndIndents

-- UTILS 

addPositions :: (Positions -> a -> b) -> WithPos a -> b
addPositions c WithPos{..} = c (Positions{..}) tokenVal

btwWithPos :: WithPos a -> WithPos a1 -> Positions
btwWithPos l r = Positions (Tok.startPos l) (Tok.endPos r)
 

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

tokWPosNoErr :: (WithPos SToken -> Maybe a) -> Parser a
tokWPosNoErr f = Parsec.token f Set.empty