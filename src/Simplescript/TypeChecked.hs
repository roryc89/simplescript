module Simplescript.TypeChecked where

import Data.Text (Text)

data TopAssignment = TopAssignment Text Expr

data Type 
    = Regular Text
    | RecordType [(Text, Type)]
    deriving (Show, Eq, Ord)

data Expr 
    = Val Val Type
    | Apply Expr Expr
    | Op Expr Expr Expr Type
    | If Expr Expr Expr Type
    | Case Expr Type [(Destructured, Expr)] Type
    | Function Text Type Expr Type
    deriving (Show, Eq, Ord)

data Val 
    = Number Double
    | Int Int
    | String Text
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