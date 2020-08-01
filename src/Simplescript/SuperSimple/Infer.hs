module Simplescript.SuperSimple.Infer where 

import Simplescript.SuperSimple.Expr
import qualified Simplescript.SuperSimple.Type as Type
import Simplescript.SuperSimple.Type (Type)

inferExprType :: Expr () -> Either TypeError (Expr Type)
inferExprType (e, _) = case e of 
    Int i -> Right $ (Int i, Type.Int)
    Bool i -> Right $ (Bool i, Type.Bool)


data TypeError
    = TypeMismatch Type Type
    | OccursCheckFailed Int Type
    deriving(Eq, Ord, Show)