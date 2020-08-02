module Simplescript.SuperSimple.Infer (inferExprType, inferExprTypeStandalone) where 

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map
import Simplescript.SuperSimple.Expr
import qualified Simplescript.SuperSimple.Type as T
import Simplescript.SuperSimple.Type (Type)
import Simplescript.SuperSimple.SubstitutionMap
import Simplescript.Infer.UsedTypes

type Bindings = Map Text (Expr Type)

inferExprTypeStandalone :: 
    Expr () -> Either TypeError (Expr Type)
inferExprTypeStandalone = fmap fst . inferExprType mempty emptySubMap 0


inferExprType :: 
    Bindings -> SubstitutionMap -> Int -> Expr () -> Either TypeError (Expr Type, Int)
inferExprType bindings subMap id (e, _) = case e of 
    Int i -> Right $ ((Int i, T.Int), id + 1)
    Bool i -> Right $ ((Bool i, T.Bool), id + 1)
    Var varName -> case Map.lookup varName bindings of 
        Nothing -> Left $ VarNotFound varName
        Just e -> Right (e, id + 1)
    Lambda arg body -> do 
        let argT = T.Id id
        ((bodyE, bodyT), nextId) <- inferExprType (Map.insert arg (Var arg, argT) bindings) subMap (id + 1) body
        pure 
            (   ( Lambda arg (bodyE, bodyT)
                , T.Function argT bodyT
                )
            ,   nextId + 1
            )


    --     let 
    --         argT = 
    --     in
    --     Right $ (Lambda )

    -- TODO
-- ids should be created when an arg is introduced and vice versa when var used

data TypeError
    = TypeMismatch Type Type
    | VarNotFound VarName
    | OccursCheckFailed Int Type
    deriving(Eq, Ord, Show)