{-# LANGUAGE DeriveFunctor       #-}

module Simplescript.Error where 

import qualified Data.Text                  as T
import qualified Text.Megaparsec            as Parsec
import           Data.Void                  (Void)
import qualified Simplescript.Token as Tok
import qualified Simplescript.Ast as Ast

data ParseOrLexError 
    = LexError (Parsec.ParseErrorBundle T.Text Void)
    | ParseError (Parsec.ParseErrorBundle Tok.TokStream Void)
    deriving (Show, Eq)

data TypeCheckError a
    = TypesDoNotUnify (Ast.Type a) (Ast.Type a)
    | VarNotFound T.Text a
    deriving (Show, Eq, Functor)

data SimpleError a
    = ParseOrLexError ParseOrLexError
    | TypeCheckError [TypeCheckError a]
    deriving (Show, Eq, Functor)