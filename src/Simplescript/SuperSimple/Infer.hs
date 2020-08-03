{-# LANGUAGE RecordWildCards #-}

module Simplescript.SuperSimple.Infer (inferExprType, inferExprTypeStandalone) where 

import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Text (Text)
import Data.List 
import qualified Data.Map as Map
import Simplescript.SuperSimple.Expr
import qualified Simplescript.SuperSimple.Type as T
import Simplescript.SuperSimple.Type (Type)
import Simplescript.SuperSimple.SubstitutionMap (SubstitutionMap)
import qualified Simplescript.SuperSimple.SubstitutionMap as SubMap
import Simplescript.Infer.UsedTypes

type ExprBindings = Map VarName (Expr Type)

type TypeBindings = Map VarName Int

data Env = Env 
    { exprs :: ExprBindings 
    , types :: TypeBindings
    , subMap :: SubstitutionMap
    , currentId :: Int
    }

emptyEnv :: Env
emptyEnv = Env mempty mempty SubMap.empty 0

insertExpr :: VarName -> Expr Type -> Env -> Env
insertExpr varName e = insertExprs [(varName, e)]

insertExprs :: [(VarName, Expr Type)] -> Env -> Env
insertExprs newExprs Env{..} =
    Env 
        (Map.union (Map.fromList newExprs) exprs)
        types
        subMap
        currentId


insertType :: VarName -> [T.Ctr] -> Env -> Env
insertType varName t Env{..} =
    Env
        exprs
        (Map.insert varName currentId types)
        undefined
        -- (SubMap.insert currentId (T.UserDefined currentId varName t) subMap)
        (currentId + 1)
    
insertTypes ::  Env -> [(VarName, [T.Ctr])] -> Env
insertTypes  = foldr (uncurry insertType)

incId :: Env -> Env
incId Env{..} = 
    Env 
        exprs
        types
        subMap
        (currentId + 1)

inferExprTypeStandalone :: 
    Expr () -> Either [TypeError] (Expr Type)
inferExprTypeStandalone = fmap fst . inferExprType emptyEnv

inferExprType :: 
   Env -> Expr () -> Either [TypeError] (Expr Type, Env)
inferExprType env@Env{..} (e, _) = case e of 

    Int i -> Right $ ((Int i, T.Int), incId env)

    Bool i -> Right $ ((Bool i, T.Bool), incId env)

    Var varName -> case Map.lookup varName exprs of 
        Nothing -> Left [VarNotFound varName]
        Just e -> Right (e, incId env)

    Lambda arg body -> do 
        let argT = T.Id currentId
        ((bodyE, bodyT), nextEnv) <- inferExprType (insertExpr arg (Var arg, argT) env) body
        pure 
            (   ( Lambda arg (bodyE, bodyT)
                , T.Function argT bodyT
                )
            ,   nextEnv
            )

    Let bs expr -> do
        let typeBindings = getTypeBindings bs
        (bsTyped, nextEnv) <- inferExprBindings (insertTypes env typeBindings) (getExprBindings bs)
        inferExprType (insertExprs bsTyped nextEnv) expr

getExprBindings :: [(VarName, ExprOrType ())] -> [(VarName, Expr ())]
getExprBindings = concatMap getExprBinding

getExprBinding :: (VarName, ExprOrType ()) -> [(VarName, Expr ())]
getExprBinding (vn, e) = case e of 
    T _ -> []
    E expr -> [(vn, expr)]

getTypeBindings :: [(VarName, ExprOrType ())] -> [(VarName, [T.Ctr])]
getTypeBindings = concatMap getTypeBinding

getTypeBinding :: (VarName, ExprOrType ()) -> [(VarName, [T.Ctr])]
getTypeBinding (vn, e) = case e of 
    T t -> [(vn, t)]
    E expr -> []
    
inferExprBindings :: Env -> [(VarName, Expr ())] -> Either [TypeError] ([(VarName, Expr Type)], Env)
inferExprBindings env [] = Right ([], env)
inferExprBindings env ((varName, expr) : t) = 
    case inferExprType env expr of 
        Left err -> Left err 
        Right (expr, nextEnv) ->
            let 
                newEnv = insertExpr varName expr env
            in 
            case inferExprBindings newEnv t of 
                Left err -> Left err 
                Right (rest, nextEnv_) -> Right $ ((varName, expr) : rest, nextEnv_)
                
data TypeError
    = TypeMismatch Type Type
    | VarNotFound VarName
    | OccursCheckFailed Int Type
    deriving(Eq, Ord, Show)