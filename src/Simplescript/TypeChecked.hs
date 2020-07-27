module Simplescript.TypeChecked where

import Data.Text (Text)

data TopAssignment = TopAssignment Text Expr

data Type 
    = Constant Text [TypeOrId]
    | TypeVar Text
    | Function TypeOrId TypeOrId
    deriving (Show, Eq, Ord)

data TypeOrId
    = Id Int
    | Type Type 
    deriving (Show, Eq, Ord)

type Expr = (ExprWoT, TypeOrId)

data ExprWoT 
    = Val Val
    | Apply Expr Expr
    | Op Expr Expr Expr
    | If Expr Expr Expr
    | Case Expr [(Destructured, Expr)]
    | Lambda Expr
    deriving (Show, Eq, Ord)

data Val 
    = Number Double
    | Int Int
    | String Text
    | Char Char
    | Record [(Text, Expr)]
    | List Expr
    | Arg Text
    deriving (Show, Eq, Ord)

data Destructured 
    = VarDes Text [Destructured] 
    | IntDes Int 
    | NumberDes Double
    | StringDes Text
    | ListDes [Destructured] 
    | RecordDes [(Text, Maybe Destructured)]
    deriving (Show, Eq, Ord)