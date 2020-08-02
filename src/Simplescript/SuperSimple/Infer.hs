module Simplescript.SuperSimple.Infer (inferExprType, inferExprTypeStandalone) where 

import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Text (Text)
import Data.List 
import qualified Data.Map as Map
import Simplescript.SuperSimple.Expr
import qualified Simplescript.SuperSimple.Type as T
import Simplescript.SuperSimple.Type (Type)
import Simplescript.SuperSimple.SubstitutionMap
import Simplescript.Infer.UsedTypes

type Bindings = Map Text (Expr Type)

inferExprTypeStandalone :: 
    Expr () -> Either [TypeError] (Expr Type)
inferExprTypeStandalone = fmap fst . inferExprType mempty emptySubMap 0


inferExprType :: 
    Bindings -> SubstitutionMap -> Int -> Expr () -> Either [TypeError] (Expr Type, Int)
inferExprType bindings subMap id (e, _) = case e of 
    Int i -> Right $ ((Int i, T.Int), id + 1)
    Bool i -> Right $ ((Bool i, T.Bool), id + 1)
    Var varName -> case Map.lookup varName bindings of 
        Nothing -> Left [VarNotFound varName]
        Just e -> Right (e, id + 1)
    Lambda arg body -> do 
        let argT = T.Id id
        ((bodyE, bodyT), nextId) <- inferExprType (Map.insert arg (Var arg, argT) bindings) subMap id body
        pure 
            (   ( Lambda arg (bodyE, bodyT)
                , T.Function argT bodyT
                )
            ,   nextId
            )

    Let bs expr -> do
        (bsTyped, nextId) <- first Map.fromList <$> inferBindings bindings subMap id bs
        inferExprType (Map.union bsTyped bindings) subMap nextId expr

inferBindings :: Bindings -> SubstitutionMap -> Int -> [(VarName, Expr ())] -> Either [TypeError] ([(VarName, Expr Type)], Int)
inferBindings bindings subMap id [] = Right ([], id)
inferBindings bindings subMap id ((varName, expr) : t) = 
    case inferExprType bindings subMap id expr of 
        Left err -> Left err 
        Right (expr, nextId) ->
            let 
                newBindings =  Map.insert varName expr bindings
            in 
            case inferBindings newBindings subMap id t of 
                Left err -> Left err 
                Right (rest, nextId_) -> Right $ ((varName, expr) : rest, nextId_)
            
            -- Right $ (expr, nextId) : inferBindings newBindings subMap id t
            -- let 
            --     (expr, nextId) = inferExprType bindings subMap (id + 1) expr
            -- in 
            -- case 

            


data TypeError
    = TypeMismatch Type Type
    | VarNotFound VarName
    | OccursCheckFailed Int Type
    deriving(Eq, Ord, Show)