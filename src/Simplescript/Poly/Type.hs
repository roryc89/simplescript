module Simplescript.Poly.Type where

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TCtr String
  | TArrow Type Type
  deriving (Show, Eq, Ord)

data Forall = Forall [TVar] Type
  deriving (Show, Eq, Ord)

typeInt :: Type
typeInt  = TCtr "Int"

typeBool :: Type
typeBool = TCtr "Bool"
