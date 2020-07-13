{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}


module Simplescript.Parse where 

import           Control.Applicative        (empty)
import           Control.Monad              (void)
import           Data.Char                  (isDigit, isAlpha, isAlphaNum)
import           Data.Text                  (unpack, Text)
import qualified Data.Text                  as T
import Data.Set as Set
import qualified Text.Read as R
import           Data.Void                  (Void)
import           NeatInterpolation          (text)
import qualified Text.Megaparsec            as Parsec
import           Text.Megaparsec            (choice, many, some, (<|>))
import qualified Text.Megaparsec.Char       as CharParser
import           Text.Megaparsec.Char       (char, string)
import qualified Text.Megaparsec.Char.Lexer as Lexer
import Simplescript.Token as Tok
import Simplescript.Ast

type Parser = Parsec.Parsec Void TokStream

-- sLex :: T.Text -> Either (Parsec.ParseErrorBundle T.Text Void) [TokenAndPosLine]
-- sLex = Parsec.runParser pLines "" 

-- pLiteral :: Parser LiteralPos
-- pLiteral = choice 
--     [ tokNoErr
--     ]
-- pDouble :: Parser (WithPos Double)
-- pDouble = tokNoErr (\case 
--       Tok.Number n -> n
--     )

tokNoErr :: (WithPos SToken -> Maybe a) -> Parser a
tokNoErr f = Parsec.token f Set.empty

tokNoErrPos :: (WithPos SToken -> Maybe a) -> Parser (a, Parsec.SourcePos, Parsec.SourcePos)
tokNoErrPos f = tokNoErrPos (\t -> 
    case f t of 
        Just tok -> Just tok
        Nothing -> Nothing)