{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE QuasiQuotes #-}

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
import           Text.Megaparsec.Char       (char)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Simplescript.Token (SToken(..), Line(..), WithPos(..), TokenAndPosLine, showSToken)

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
        t <- Parsec.lookAhead $ Parsec.takeWhileP Nothing (/= '\n')
        tokens <- pSTokens
        return $ Lexer.IndentMany Nothing (return . Line t tokens) pLine

-- lexeme 

lineComment :: Parser ()
lineComment = Lexer.skipLineComment "#"

spacesNewlines :: Parser ()
spacesNewlines = Lexer.space CharParser.space1 lineComment empty

spaces :: Parser ()
spaces = Lexer.space (void takeLine) lineComment empty

takeLine :: Parser Text
takeLine = Parsec.takeWhile1P Nothing pred
  where
    pred c = c == ' ' || c == '\t'

scl :: Parser ()
scl = Lexer.space (void $ some (CharParser.char ' ' <|> CharParser.char '\t' <|> CharParser.char '\n')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = Lexer.lexeme scl

-- tokens

pSTokens :: Parser [WithPos SToken]
pSTokens = Parsec.manyTill pSToken (Parsec.eof <|> void CharParser.eol)

pSToken :: Parser (WithPos SToken) 
pSToken = withPos $ lexeme $ choice 
    [ Assign <$ char '='
    , Backslash <$ char '\\'
    , Colon <$ char ':'
    , Comma <$ char ','
    , LParen <$ char '('
    , RParen <$ char ')'
    , LBrace <$ char '{'
    , RBrace <$ char '}'
    , LSquareBracket <$ char '['
    , RSquareBracket <$ char ']'
    , pString
    , Parsec.try pNumber 
    , pInt
    , pIdentifier
    , Operator . T.pack <$> some (choice $ fmap char ("$%^&*-+=;<>,./~!" :: String))
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

