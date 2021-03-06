{-# LANGUAGE OverloadedStrings #-}

module Simplescript.Lex where

import           Control.Applicative        (empty)
import           Control.Monad              (void)
import           Data.Char                  (isDigit, isAlpha, isAlphaNum)
import           Data.Text                  (unpack, Text)
import qualified Data.Text                  as T
import qualified Text.Read as R
import           Data.Void                  (Void)
import           NeatInterpolation          (text)
import qualified Text.Megaparsec            as Parsec
import           Text.Megaparsec            (choice, many, some, (<|>))
import qualified Text.Megaparsec.Char       as CharParser
import           Text.Megaparsec.Char       (char, string)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Simplescript.Token (SToken(..), Keyword(..), Line(..), WithPos(..), TokenAndPosLine, showSToken)

type Parser = Parsec.Parsec Void Text

sLex :: T.Text -> Either (Parsec.ParseErrorBundle T.Text Void) [TokenAndPosLine]
sLex = Parsec.runParser pLines "" 

-- lines 

pLines :: Parser [TokenAndPosLine]
pLines = Parsec.manyTill pLine Parsec.eof

pLine :: Parser TokenAndPosLine
pLine = Lexer.indentBlock spacesNewlines p
  where
    p = do
        tokens <- pSTokens
        return $ Lexer.IndentMany Nothing (return . Line tokens) pLine

-- lexeme 

spacesNewlines :: Parser ()
spacesNewlines = Lexer.space CharParser.space1 lineComment empty

spaces :: Parser ()
spaces = Lexer.space (void takeLine) lineComment empty

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "//"

takeLine :: Parser Text
takeLine = Parsec.takeWhile1P Nothing pred
  where
    pred c = c == ' ' || c == '\t'

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme spaces

-- tokens

pSTokens :: Parser [WithPos SToken]
pSTokens = Parsec.many pSToken

pSToken :: Parser (WithPos SToken) 
pSToken = lexeme $ withPos $ choice 
    [ Arrow <$ string "=>"
    , Keyword <$> pKeyword
    , Assign <$ char '='
    , Backslash <$ char '\\'
    , Colon <$ char ':'
    , Comma <$ char ','
    , Pipe <$ char '|'
    , LParen <$ char '('
    , RParen <$ char ')'
    , LBrace <$ char '{'
    , RBrace <$ char '}'
    , LSquareBracket <$ char '['
    , RSquareBracket <$ char ']'
    , pString
    , pChar
    , Parsec.try pNumber 
    , pInt
    , pIdentifier
    , Operator . T.pack <$> some (choice $ fmap char ("$%^&*-+=;<>,./~!" :: String))
    ]

pKeyword :: Parser Keyword
pKeyword = choice 
    [ Type <$ string "type" 
    , Let <$ string "let" 
    , In <$ string "in" 
    , Case <$ string "case" 
    , Of <$ string "of" 
    , If <$ string "if" 
    , Is <$ string "is" 
    , Then <$ string "then" 
    , Else <$ string "else" 
    , Import <$ string "import" 
    , Export <$ string "export" 
    , Help <$ string "help" 
    ]

withPos :: Parser SToken -> Parser (WithPos SToken)
withPos p = do 
    startPos <- Parsec.getSourcePos 
    tokenVal <- p
    endPos <- Parsec.getSourcePos 
    return $ WithPos startPos endPos (T.length $ showSToken tokenVal) tokenVal
    

pString :: Parser SToken
pString = SString . T.pack
    <$> Parsec.between (char '"') (char '"') (many (Parsec.satisfy (/= '"')))

pIdentifier :: Parser SToken
pIdentifier = do 
    first <- Parsec.satisfy isAlpha
    rest <- many CharParser.alphaNumChar 
    pure $ Identifier $ T.pack $ first : rest 

pInt :: Parser SToken
pInt = do
    str <- pDigitsStr
    case R.readEither str of 
        Right n -> pure $ Int n
        Left err -> fail err

pChar :: Parser SToken
pChar = do
    CharParser.char '\''
    c <- CharParser.asciiChar
    CharParser.char '\''
    return $ Char c

pNumber :: Parser SToken
pNumber = do
    pre <- pDigitsStr
    point <- char '.'
    after <- pDigitsStr
    case R.readEither $ pre <> (point : after) of 
        Right n -> pure $ Number n
        Left err -> fail err

pDigitsStr :: Parser String
pDigitsStr = some (Parsec.satisfy isDigit)

