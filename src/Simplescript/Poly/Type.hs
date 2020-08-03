module Simplescript.Poly.Type where

newtype TVar = TV String
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar -- Type variables such as `a`
  | TCtr String -- Type Constructor for creating types such as `Int`
  | TArrow Type Type -- Type arrow for functions
  deriving (Show, Eq, Ord)

data Forall = Forall [TVar] Type
  deriving (Show, Eq, Ord)

typeInt :: Type
typeInt  = TCtr "Int"

typeBool :: Type
typeBool = TCtr "Bool"
