{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable       #-}

module Simplescript.Ast where

import Data.Text (Text)
import Text.Megaparsec.Pos (SourcePos)


type ModulePos = Module Positions

newtype Module a 
    = Module 
    { statements :: [Statement a] 
    }
    deriving (Show, Eq, Ord, Functor, Foldable)

type StatementPos = Statement Positions

data Statement a
    = TypeAnnotation a Text (Type a)
    | TypeDeclaration a Text [(Text, a)] [Ctr a]
    | VarDeclaration a Text (Expr a)
    deriving (Show, Eq, Ord, Functor, Foldable)


type TypePos = Type Positions

data Type a
    = TypeIdentifier a Text
    | TypeApply (Type a) (Type a)
    | TypeOp a Text (Type a) (Type a)
    | TypeParens a (Type a)
    | TypeLit (TypeLiteral a)
    deriving (Show, Eq, Ord, Functor, Foldable)

type TypeLiteralPos = TypeLiteral Positions

data TypeLiteral a
    = StringTypeLit a Text 
    | ListTypeLit a [Type a]
    | RecordTypeLit a [(Text, Type a)]
    deriving (Show, Eq, Ord, Functor, Foldable)

type ExprPos = Expr Positions

data Expr a
    = Var a Text (Maybe (Type a))
    | Apply (Expr a) (Expr a)
    | Op a Text (Expr a) (Expr a)
    | Parens a (Expr a) (Maybe (Type a))
    | Let a [Statement a] (Expr a)
    | If a (Expr a) (Expr a) (Expr a)
    | Case a (Expr a) [(Destructured a, Expr a)] (Maybe (Type a))
    | Lit (Literal a) (Maybe (Type a))
    deriving (Show, Eq, Ord, Functor, Foldable)

type LiteralPos = Literal Positions

data Literal a
    = IntLit a Int
    | NumberLit a Double
    | StringLit a Text 
    | ListLit a [Expr a]
    | RecordLit a [(Text, Expr a)]
    | FunctionLit a [Destructured a] (Expr a)
    deriving (Show, Eq, Ord, Functor, Foldable)

type CtrPos = Ctr Positions

data Ctr a = Ctr a Text [Type a]
    deriving (Show, Eq, Ord, Functor, Foldable)

data Positions = Positions 
    { startPos :: SourcePos
    , endPos :: SourcePos
    }    
    deriving (Show, Eq, Ord)

type DestructuredPos = Destructured Positions

data Destructured a 
  = VarDes a Text [Destructured a] (Maybe (Type a))
  | IntDes a Int (Maybe (Type a))
  | NumberDes a Double (Maybe (Type a))
  | StringDes a Text (Maybe (Type a))
  | ListDes a [Destructured a] (Maybe (Type a))
  | RecordDes a [(Text, a, Maybe (Destructured a))] (Maybe (Type a))
    deriving (Show, Eq, Ord, Functor, Foldable)

