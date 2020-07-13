{-# LANGUAGE DeriveFunctor       #-}

module Simplescript.Ast where

import Data.Text (Text)
import Text.Megaparsec.Pos (SourcePos)


type ModulePos = Module Positions

newtype Module a 
    = Module 
    { statements :: [Statement a] 
    }
    deriving (Show, Eq, Ord, Functor)

type StatementPos = Statement Positions

data Statement a
    = TypeAnnotation a Text (Type a)
    | VarDeclaration a Text (Expr a)
    deriving (Show, Eq, Ord, Functor)

type TypePos = Type Positions

data Type a
    = TypeIdentifier a Text
    | TypeApply a (Type a) (Type a)
    | TypeOperator a Text (Type a) (Type a)
    deriving (Show, Eq, Ord, Functor)

type ExprPos = Expr Positions

data Expr a
    = Var a Text
    | Apply a (Expr a) (Expr a)
    | Parens a (Expr a)
    | Let a [Statement a] (Expr a)
    | Lit (Literal a)
    deriving (Show, Eq, Ord, Functor)

type LiteralPos = Literal Positions

data Literal a
    = IntLit a Int
    | NumberLit a Double
    | StringLit a Text 
    | ListLit a [Literal a]
    | RecordLit a [(Text, Expr a)]
    | FunctionLit a [Text] (Expr a) 
    deriving (Show, Eq, Ord, Functor)

data Positions = Positions 
    { startPos :: SourcePos
    , endPos :: SourcePos
    }    
    deriving (Show, Eq, Ord)
